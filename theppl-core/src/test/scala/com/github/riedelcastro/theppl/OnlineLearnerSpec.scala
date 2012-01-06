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
      case class Data(x: Int, y: Boolean)
      case class Label(data: Data) extends Variable[Boolean]

      val tokens = Seq(Data(0, true), Data(1, false))
      val instances = tokens.map(t => Instance(t, Label(t) -> t.y))
      val dom = tokens.map(_.y).toSet.toSeq

      val classifier = new Classifier[Boolean, Data] {
        val domain = dom
        def labelFeatures(label: Label) = Feat(label)
        def contextFeatures(context: Context) = Feat(context.x)
        def variable(context: Context) = Label(context)
      }
      val learner = new classifier.decorated with OnlineLearner with PerceptronUpdate
      learner.train(instances)
    }
  }

}