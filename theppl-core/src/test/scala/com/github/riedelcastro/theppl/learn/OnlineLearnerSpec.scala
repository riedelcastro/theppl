package com.github.riedelcastro.theppl.learn

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import com.github.riedelcastro.theppl._
import Imports._
import infer.Argmaxer

/**
 * @author sriedel
 */
class OnlineLearnerSpec extends Spec with MustMatchers {

  describe("An OnlineLearner") {
    it("should separate separable data with the Perceptron update rule") {
      case class Data(x: Int, y: Boolean)
      case class LabelVar(data: Data) extends BoolVariable
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1,data2)
      trait TestClassifier extends Classifier[Data] {
        type Context = Data
        type LabelType = Boolean
        type LabelVariableType = LabelVar
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context:Context) = vector(context.x)
        def variable(context: Context) = LabelVar(context)
      }

      val classifier = new TestClassifier {}
      val learner = new OnlineLearner[Data] with PerceptronUpdate {
        val template = classifier
        def targetState(context: Data, potential: template.PotentialType) = potential.labelVariable -> context.y
        def argmaxer(potential: template.PotentialType) = Argmaxer(potential)
        def instances = tokens
      }

      learner.train()

      //classifier.predict(data1)(LabelVar(data1))
      Argmaxer(classifier.potential(data1)).predict(LabelVar(data1)) must be (data1.y)
      Argmaxer(classifier.potential(data2)).predict(LabelVar(data2)) must be (data2.y)
    }
  }

  describe("An OnlineLearner2") {
    it("should separate separable data with the Perceptron update rule") {
      case class Data(x: Int, y: Boolean)
      case class LabelVar(data: Data) extends BoolVariable
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1,data2)

      val classifier = new Classifier[Data] {
        type Context = Data
        type LabelType = Boolean
        type LabelVariableType = LabelVar
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context:Context) = vector(context.x)
        def variable(context: Context) = LabelVar(context)
        override def truth(context: Data) = Some(context.y)
      }

      val learner = new OnlineLearner2[Data] with PerceptronUpdate {
        val template = classifier
        def argmaxer(potential: template.PotentialType) = Argmaxer(potential)
        def instances = tokens
      }

      learner.train()

      //classifier.predict(data1)(LabelVar(data1))
      classifier.predict(data1).value(LabelVar(data1)) must be (data1.y)
      classifier.predict(data2).value(LabelVar(data1)) must be (data2.y)
    }
  }


}