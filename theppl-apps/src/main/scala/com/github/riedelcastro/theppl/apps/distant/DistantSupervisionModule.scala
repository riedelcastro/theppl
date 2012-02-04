package com.github.riedelcastro.theppl.apps.distant

import com.github.riedelcastro.theppl._
import util.Util
import Util._


/**
 * @author sriedel
 */
trait DistantSupervisionModule[EntityType] extends LinearModule[EntityType] {
  self =>
  type MentionType
  type EntityVariableType <: Variable[Boolean]
  type ModelType <: DistantSupervisionModel

  val weights = new ParameterVector

  case class BoolVar[T](id: T) extends BoolVariable

  def entityVariable(entity: EntityType): EntityVariableType
  def mentions(entity: EntityType): Seq[MentionType]

  def entityFeatures(entity: EntityType): ParameterVector
  def mentionFeatures(mention: MentionType, entity:EntityType): ParameterVector

  trait DistantSupervisionModel extends super.LinearModel {

    def entity: EntityType

    lazy val hiddenEntity = entityVariable(entity)

  }


}


/**
 * @author sriedel
 */
trait DistantSupervisionClassifier[EntityType] extends DistantSupervisionModule[EntityType] {

  type ModelType = ClassifierModel

  trait ClassifierModel extends DistantSupervisionModel {

    lazy val hidden = Seq(hiddenEntity)
    def features(state: State) = {
      val result = new ParameterVector()
      if (state(hiddenEntity)) {
        result.add(entityFeatures(entity),1.0)
        for (mention <- mentions(entity)) {
          result.add(mentionFeatures(mention, entity),1.0)
        }
      }
      result
    }
  }

  def model(c: EntityType) = new ClassifierModel {
    def entity = c
  }

}

trait EntityMentionModule[EntityType] extends DistantSupervisionModule[EntityType] { module =>
  type MentionVariableType <: Variable[Boolean]

  def mentionVariable(mention: MentionType): MentionVariableType

  type ModelType <: EntityMentionModel

  trait EntityMentionModel extends DistantSupervisionModel {
    latentModel =>

    lazy val mentions = module.mentions(entity).toIndexedSeq
    lazy val hiddenMentions = mentions.map(mentionVariable(_))
    lazy val hidden = (hiddenMentions :+ hiddenEntity)
    lazy val entFeats = entityFeatures(entity)
    lazy val feats = mentions.map(m => mentionFeatures(m,entity)).toArray

    def features(state: State) = {
      val result = new ParameterVector()
      if (state(hiddenEntity)) result.add(entFeats, 1.0)
      forIndex(mentions.size) {
        i => if (state(hiddenMentions(i))) result.add(feats(i), 1.0)
      }
      result
    }
  }

}
trait LatentDistantSupervisionModule[EntityType] extends DistantSupervisionModule[EntityType] {
  module =>


  type MentionVariableType <: Variable[Boolean]
  type ModelType = LatentModel

  def mentionVariable(mention: MentionType): MentionVariableType

  def model(context: EntityType) = new LatentModel {def entity = context}

  trait LatentModel extends DistantSupervisionModel {
    latentModel =>

    import math._

    lazy val mentions = module.mentions(entity).toIndexedSeq
    lazy val hiddenMentions = mentions.map(mentionVariable(_))
    lazy val hidden = (hiddenMentions :+ hiddenEntity)
    lazy val entFeats = entityFeatures(entity)
    lazy val feats = mentions.map(m => mentionFeatures(m,entity)).toArray


    override def score(state: State) = {
      if (!state(hiddenEntity) && hiddenMentions.exists(state(_))) Double.NegativeInfinity else super.score(state)
    }

    def features(state: State) = {
      val result = new ParameterVector()
      if (state(hiddenEntity)) result.add(entFeats, 1.0)
      forIndex(mentions.size) {
        i => if (state(hiddenMentions(i))) result.add(feats(i), 1.0)
      }
      result
    }

    override def defaultExpectator(cookbook: ExpectatorRecipe[com.github.riedelcastro.theppl.Model]) = new Expectator {
      val model = latentModel
      lazy val entScore = entFeats dot weights
      lazy val scores = feats.map(_ dot weights)

      def expectations(penalties: Messages) = {
        val n = mentions.size
        val logZs = Array.ofDim[Double](n)
        val logMentionMargs = Array.ofDim[Double](n)

        //convert penalties into array
        val mentionPenalties = hiddenMentions.map(m => penalties(m, true) - penalties(m, false)).toArray
        val entityPenalty = penalties(hiddenEntity, true) - penalties(hiddenEntity, false)
        var tmpZ = entScore + entityPenalty

        //log partition function and local log partition functions
        forIndex(n) {
          i =>
            logZs(i) = log1p(exp(scores(i) + mentionPenalties(i)))
            tmpZ += logZs(i)
        }
        val lZ = log1p(exp(tmpZ))

        //mention marginals
        forIndex(n) {
          i =>
            logMentionMargs(i) = tmpZ - logZs(i) + scores(i) + mentionPenalties(i) - lZ
        }

        //entity marginal
        val logEntMarg = tmpZ - lZ

        //prepare messages---UGLY
        val mentionMsgs: Map[Variable[Any], Message[Boolean]] =
          Range(0, n).view.map(i => hiddenMentions(i) ->
            Message.binary(hiddenMentions(i), logMentionMargs(i), tmpZ - logZs(i) - lZ)).toMap
        val entityMsg = Message.binary(hiddenEntity, logEntMarg, -lZ)
        val result = new Messages {
          def message[V](variable: Variable[V]) = variable match {
            case x if (x == hiddenEntity) => entityMsg.asInstanceOf[Message[V]]
            case m => mentionMsgs(m).asInstanceOf[Message[V]]
          }
        }

        //calculate feature expectations
        def featExp() = {
          val result = new ParameterVector()
          result.add(entFeats, exp(logEntMarg))
          forIndex(n) {
            i =>
              result.add(feats(i), exp(logMentionMargs(i)))
          }
          result
        }

        new Expectations {
          lazy val featureExpectations = featExp()
          lazy val logMarginals = result
          def logZ = lZ
        }
      }
    }
  }

}

trait PreferKMentions[EntityType] extends LatentDistantSupervisionModule[EntityType] {
  def entityFeatures(entity: EntityType) = ParameterVector(Map(entity -> mentions(entity).size))
  def mentionFeatures(mention: MentionType, entity:EntityType) = ParameterVector(Map(entity -> -1.0))
}





