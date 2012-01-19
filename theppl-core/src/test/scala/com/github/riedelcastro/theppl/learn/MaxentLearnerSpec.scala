package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import Imports._



/**
 * @author sriedel
 */
class MaxentLearnerSpec extends ThePPLSpec {

  describe("A MaxentLearner") {
    it("should yield a model that has the same expectations as the training data") {
      case class Data(x: Int, y: Boolean)
      case class Label(data: Data) extends Variable[Boolean]
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1, data2)
      val instances = tokens.map(t => Instance(t, Label(t) -> t.y))
      val dom = tokens.map(_.y).toSet.toSeq
      val classifier = Classifier((d: Data) => Label(d), dom, (d: Data) => vector(d.x), (l: Boolean) => vector(l))
      val learner = new classifier.decorated with MaxentLearner
      learner.train(instances)

      val delta = new HierarchicalParameterVector()

      for (i <- instances; model = classifier.model(i.context, i.observation)) {
        val gold = model.features(i.gold)
        val guess = model.expectations(Message.empty).featureExpectations
        delta.add(gold, 1.0)
        delta.add(guess, -1.0)
      }
      for ((path,feats) <- delta.params; (_,value) <- feats.values)
        value must be (0.0 plusOrMinus epsLarge)

    }
  }

}