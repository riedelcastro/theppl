package com.github.riedelcastro.theppl.apps.distant

import com.github.riedelcastro.theppl.{Feat, BruteForceExpectator, BoolVariable, ThePPLSpec}


/**
 * @author sriedel
 */
class LatentDistantSupervisionSpec extends ThePPLSpec {
  
  describe("A latent distant supervision model") {
    it ("should provide exact expecations with the default expectator") {
      case class Mention(feats:Seq[String])
      case class Entity(feats:Seq[String],mentions:Seq[Mention])
      case class Var[Id](id:Id) extends BoolVariable
      
      val mentions = Range(0,3).map(i => Mention(Seq("mention")))
      val entity = Entity(Seq("entity"),mentions)
      
      trait Module extends LatentDistantSupervisionModule[Entity] {
        type MentionType = Mention
        type EntityVariableType = Var[Entity]
        type MentionVariableType = Var[Mention]
        def entityVariable(entity: Entity) = Var(entity)
        def entityFeatures(entity: Entity) = entity.feats
        def mentions(entity: Entity) = entity.mentions
        def mentionFeatures(mention: Module#MentionType) = mention.feats
        def mentionVariable(mention: Module#MentionType) = Var(mention)
      }
      val module = new Module {}
      module.weights(Feat("mention")) = 1.0
      module.weights(Feat("entity")) = 1.0
      val model = module.model(entity)
      val bf = BruteForceExpectator.expectator(model)
      val default = model.defaultExpectator()
      
    }
  }

}