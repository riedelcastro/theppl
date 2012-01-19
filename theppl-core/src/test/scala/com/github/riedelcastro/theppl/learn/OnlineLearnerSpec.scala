package com.github.riedelcastro.theppl.learn

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import com.github.riedelcastro.theppl.{Classifier, Variable, Imports}
import Imports._

/**
 * @author sriedel
 */
class OnlineLearnerSpec extends Spec with MustMatchers {

  describe("An OnlineLearner") {
    it("should separate separable data with the Perceptron update rule") {
      case class Data(x: Int, y: Boolean)
      case class Label(data: Data) extends Variable[Boolean]
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1,data2)
      val instances = tokens.map(t => Instance(t, Label(t) -> t.y))
      val dom = tokens.map(_.y).toSet.toSeq
      val classifier = Classifier((d:Data) => Label(d), dom, (d:Data) => vector(d.x), (l:Boolean) => vector(l))
      val learner = new classifier.decorated with OnlineLearner with PerceptronUpdate
      learner.train(instances)
      classifier.model(data1).predict(Label(data1)) must be (data1.y)
      classifier.model(data2).predict(Label(data2)) must be (data2.y)
    }
  }

}