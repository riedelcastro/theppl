package com.github.riedelcastro.theppl.learn

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import com.github.riedelcastro.theppl._
import Imports._

/**
 * @author sriedel
 */
class OnlineLearnerSpec extends FunSpec with MustMatchers {

  describe("An OnlineLearner") {
    it("should separate separable data with the Perceptron update rule") {
      case class Data(x: Int, y: Boolean)
      case class LabelVar(data: Data) extends BoolVariable
      val data1 = Data(0, y = true)
      val data2 = Data(1, y = false)
      val tokens = Seq(data1,data2)

      val classifier = new Classifier[Data] {
        type LabelType = Boolean
        type LabelVariableType = LabelVar
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context:Context) = vector(context.x)
        def variable(context: Context) = LabelVar(context)
        override def truth(context: Data) = Some(context.y)
      }

      val learner = new OnlineLearner[Data] with PerceptronUpdate {
        val template = classifier
        def instances = tokens
      }

      learner.train()

      //classifier.predict(data1)(LabelVar(data1))
      classifier.predict(data1).value(LabelVar(data1)) must be (data1.y)
      classifier.predict(data2).value(LabelVar(data2)) must be (data2.y)
    }
  }


}