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
      case class LabelVar(data: Data) extends Variable[Boolean]
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1, data2)
      trait TestClassifier extends Classifier with MaxentLearner {
        type Context = Data
        type LabelType = Boolean
        type LabelVariableType = LabelVar
        lazy val domain = tokens.map(_.y).toSet.toSeq
        def variable(context: Data) = LabelVar(context)
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context: Context) = vector(context.x)
        def target(model: ModelType) = model.labelVariable -> model.labelVariable.data.y
      }
      val classifier = new TestClassifier {}
      classifier.train(tokens)

      val delta = new HierarchicalParameterVector()

      for (i <- tokens; model = classifier.model(i)) {
        val goldState = classifier.target(model)
        val gold = model.features(goldState)
        val guess = model.expectations(Message.empty).featureExpectations
        delta.add(gold, 1.0)
        delta.add(guess, -1.0)
      }
      for ((path,feats) <- delta.params; (_,value) <- feats.values)
        value must be (0.0 plusOrMinus epsLarge)

    }
  }

}