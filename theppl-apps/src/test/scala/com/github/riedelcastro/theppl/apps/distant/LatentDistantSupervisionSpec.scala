package com.github.riedelcastro.theppl.apps.distant

import com.github.riedelcastro.theppl._
import infer.BruteForceExpectator


/**
 * @author sriedel
 */
class LatentDistantSupervisionSpec extends ThePPLSpec {

  describe("A latent distant supervision potential") {
    it("should provide exact expectations with the default expectator and brute force expectator") {
      case class Mention(index: Int, feats: Seq[String])
      case class Entity(feats: Seq[String], mentions: Seq[Mention])
      case class Var[Id](id: Id) extends BoolVariable

      val mentions = Range(0, 2).map(i => Mention(i, Seq("mention")))
      val entity = Entity(Seq("entity"), mentions)

      trait Template extends LatentDistantSupervisionTemplate[Entity] {
        type MentionType = Mention
        type EntityVariableType = Var[Entity]
        type MentionVariableType = Var[Mention]
        def entityVariable(entity: Entity) = Var(entity)
        def entityFeatures(entity: Entity) = new ParameterVector(entity.feats)
        def mentions(entity: Entity) = entity.mentions
        def mentionFeatures(mention: MentionType, entity:Entity) = new ParameterVector(mention.feats)
        def mentionVariable(mention: MentionType) = Var(mention)
      }
      val template = new Template {}
      template.weights("mention") = 1.0
      template.weights("entity") = 1.0
      val potential = template.potential(entity)
      val bf = BruteForceExpectator.expectator(potential).expectations()
      val default = potential.defaultExpectator().expectations()

      import math._
      val logZ = log(1 + exp(1) + 2 * exp(2) + exp(3))
      val logProbE = log(exp(1) + 2 * exp(2) + exp(3)) - logZ
      val logProbM = log(exp(2) + exp(3)) - logZ

      bf.logZ must be(logZ plusOrMinus eps)
      default.logZ must be(logZ plusOrMinus eps)

      bf.logMarginals(potential.hiddenEntity, true) must be(logProbE plusOrMinus eps)
      default.logMarginals(potential.hiddenEntity, true) must be(logProbE plusOrMinus eps)

      for (i <- 0 until entity.mentions.size) {
        bf.logMarginals(potential.hiddenMentions(i), true) must be(logProbM plusOrMinus eps)
        default.logMarginals(potential.hiddenMentions(i), true) must be(logProbM plusOrMinus eps)
      }

      bf.featureExpectations(Feat("mention")) must be (default.featureExpectations(Feat("mention")) plusOrMinus eps)
      bf.featureExpectations(Feat("entity")) must be (default.featureExpectations(Feat("entity")) plusOrMinus eps)

    }

    it("should provide exact bidirectional expectations with the default expectator and brute force expectator") {
      case class Mention(index: Int, feats: Seq[String])
      case class Entity(feats: Seq[String], mentions: Seq[Mention])
      case class Var[Id](id: Id) extends BoolVariable

      val mentions = Range(0, 2).map(i => Mention(i, Seq("mention")))
      val entity = Entity(Seq("entity"), mentions)

      trait Template extends LatentDistantSupervisionTemplate[Entity] {
        type MentionType = Mention
        type EntityVariableType = Var[Entity]
        type MentionVariableType = Var[Mention]
        def entityVariable(entity: Entity) = Var(entity)
        def entityFeatures(entity: Entity) = new ParameterVector(entity.feats)
        def mentions(entity: Entity) = entity.mentions
        def mentionFeatures(mention: MentionType, entity:Entity) = new ParameterVector(mention.feats)
        def mentionVariable(mention: MentionType) = Var(mention)
        override def bidirectional = true
      }
      val template = new Template {}
      template.weights("mention") = 1.0
      template.weights("entity") = 1.0
      val potential = template.potential(entity)
      val bf = BruteForceExpectator.expectator(potential).expectations()
      val default = potential.defaultExpectator().expectations()

      import math._
      val logZ = log(1 + 2 * exp(2) + exp(3))
      val logProbE = log(2 * exp(2) + exp(3)) - logZ
      val logProbM = log(exp(2) + exp(3)) - logZ

      bf.logZ must be(logZ plusOrMinus eps)
      default.logZ must be(logZ plusOrMinus eps)

      bf.logMarginals(potential.hiddenEntity, true) must be(logProbE plusOrMinus eps)
      default.logMarginals(potential.hiddenEntity, true) must be(logProbE plusOrMinus eps)

      for (i <- 0 until entity.mentions.size) {
        bf.logMarginals(potential.hiddenMentions(i), true) must be(logProbM plusOrMinus eps)
        default.logMarginals(potential.hiddenMentions(i), true) must be(logProbM plusOrMinus eps)
      }

      bf.featureExpectations(Feat("mention")) must be (default.featureExpectations(Feat("mention")) plusOrMinus eps)
      bf.featureExpectations(Feat("entity")) must be (default.featureExpectations(Feat("entity")) plusOrMinus eps)

    }

  }

}

