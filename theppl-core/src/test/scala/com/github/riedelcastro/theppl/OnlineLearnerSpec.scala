package com.github.riedelcastro.theppl

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import Implicits._

/**
 * @author sriedel
 */
class OnlineLearnerSpec extends Spec with MustMatchers {

  describe("An OnlineLearner") {
    it("should separate separable data with the Perceptron update rule") {
      case class Token(word:String, tag:String)
      case class TagVar(token:Token) extends Var[String,Token](token)
      val tokens = Seq(Token("Separable","ADJ"), Token("data","NN"))
      val dom = tokens.map(_.tag).toSet.toSeq
      val tagVars = tokens.map(t => TagVar(t))
      val instances = tagVars.map(t => Instance(t,t -> t.token.tag))
      val classifier = new Classifier[String, TagVar] {
        val domain = dom
        def labelFeatures(label: Label) = Feat(label)
        def contextFeatures(context: Context) = Feat(context.token.word)
        def variable(context: Context) = context
      }
      val learner = new classifier.decorated with OnlineLearner with PerceptronUpdate
      learner.train(instances)
    }
  }

}