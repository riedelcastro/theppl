package com.github.riedelcastro.theppl.apps.distant

import com.github.riedelcastro.theppl._
import infer._
import util.Util
import Util._
import math._


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
  def mentionFeatures(mention: MentionType, entity: EntityType): ParameterVector

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
        result.add(entityFeatures(entity), 1.0)
        for (mention <- mentions(entity)) {
          result.add(mentionFeatures(mention, entity), 1.0)
        }
      }
      result
    }
  }

  def model(c: EntityType) = new ClassifierModel {
    def entity = c
  }

}

trait EntityMentionModule[EntityType] extends DistantSupervisionModule[EntityType] {
  module =>
  type MentionVariableType <: Variable[Boolean]

  def mentionVariable(mention: MentionType): MentionVariableType

  type ModelType <: EntityMentionModel

  trait EntityMentionModel extends DistantSupervisionModel {
    latentModel =>

    lazy val mentions = module.mentions(entity).toIndexedSeq
    lazy val hiddenMentions = mentions.map(mentionVariable(_))
    lazy val hidden = (hiddenMentions :+ hiddenEntity)
    lazy val entFeats = entityFeatures(entity)
    lazy val feats = mentions.map(m => mentionFeatures(m, entity)).toArray

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
trait LatentDistantSupervisionModule[EntityType] extends EntityMentionModule[EntityType] {
  module =>


  type ModelType = LatentModel

  def mentionVariable(mention: MentionType): MentionVariableType

  def model(context: EntityType) = new LatentModel {def entity = context}

  trait LatentModel extends EntityMentionModel {
    latentModel =>


    override def score(state: State) = {
      if (!state(hiddenEntity) && hiddenMentions.exists(state(_))) Double.NegativeInfinity else super.score(state)
    }


    override def defaultArgmaxer(cookbook: ArgmaxRecipe[Model]) = new Argmaxer {
      val model = latentModel
      def argmax(penalties: Messages) = {
        val entScore = entFeats dot weights
        val scores = feats.map(_ dot weights)
        val n = mentions.size
        val mentionPenalties = hiddenMentions.map(m => penalties(m, true) - penalties(m, false)).toArray
        val entityPenalty = penalties(hiddenEntity, true) - penalties(hiddenEntity, false)
        val finalEntScore = entScore + entityPenalty
        val penalizedMentionScores = scores.zip(mentionPenalties).map(pair => pair._1 + pair._2)
        val activeMentions = penalizedMentionScores.map(_ > 0.0)


        //check whether the score of all active mentions is higher than the negative entity score
        //if so, the entity is active and we allow positive mentions, otherwise everything has to be inactive
        val activeMentionScore = penalizedMentionScores.view.filter(_ > 0.0).sum
        if (activeMentionScore > -finalEntScore) {
          new ArgmaxResult {
            def score = activeMentionScore + finalEntScore
            def state = State(model.hiddenEntity +: model.hiddenMentions, true +: activeMentions)
          }
        } else {
          new ArgmaxResult {
            def score = math.max(finalEntScore, 0.0)
            def state = State(model.hiddenEntity +: model.hiddenMentions, (finalEntScore > 0) +: Array.fill(n)(false))
          }
        }
      }
    }
    override def defaultExpectator(cookbook: ExpectatorRecipe[Model]) = new Expectator {
      val model = latentModel

      def expectations(penalties: Messages) = {
        val entScore = entFeats dot weights
        val scores = feats.map(_ dot weights)
        val n = mentions.size
        val logZs = Array.ofDim[Double](n)
        val logMentionMargs = Array.ofDim[Double](n)

        //convert penalties into array
        val mentionPenaltiesTrue = hiddenMentions.map(m => penalties(m, true)).toArray
        val mentionPenaltiesFalse = hiddenMentions.map(m => penalties(m, false)).toArray
        val allInactiveMentionScore = mentionPenaltiesFalse.sum
        val mentionPenalties = hiddenMentions.map(m => penalties(m, true) - penalties(m, false)).toArray
        val entityPenalty = penalties(hiddenEntity, true) - penalties(hiddenEntity, false)
        val entityPenaltyTrue = penalties(hiddenEntity, true)
        val entityPenaltyFalse = penalties(hiddenEntity, false)
        val allInactiveScore = allInactiveMentionScore + entityPenaltyFalse

        var tmpZ = entScore + entityPenaltyTrue

        //log partition function and local log partition functions
        forIndex(n) {
          i =>
            logZs(i) = log1p(exp(scores(i) + mentionPenaltiesTrue(i)) + exp(mentionPenaltiesFalse(i)) - 1)
            tmpZ += logZs(i)
        }
        val lZ = log1p(exp(tmpZ) + exp(entityPenaltyFalse) - 1)

        //mention marginals
        forIndex(n) {
          i =>
            logMentionMargs(i) = tmpZ - logZs(i) + scores(i) + mentionPenaltiesTrue(i) - lZ
        }

        //entity marginal
        val logEntMarg = tmpZ - lZ

        //prepare messages---UGLY
        val mentionMsgs: Map[Variable[Any], Message[Boolean]] =
          Range(0, n).view.map(i => hiddenMentions(i) ->
            Message.binary(hiddenMentions(i), logMentionMargs(i), log1p(-exp(logMentionMargs(i))))).toMap
        val entityMsg = Message.binary(hiddenEntity, logEntMarg, log1p(-exp(logEntMarg)))
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

trait SomeFractionActive[EntityType] extends LatentDistantSupervisionModule[EntityType] {
  def fractionActive = 0.3

  def reliability(entity: EntityType): Double

  def entityFeatures(entity: EntityType) =
    ParameterVector.fromMap(Map(entity -> (mentions(entity).size * 0.0), ('target, entity) -> reliability(entity)))

  def mentionFeatures(mention: MentionType, entity: EntityType) =
    ParameterVector.fromMap(Map(entity -> -0.0))

}





