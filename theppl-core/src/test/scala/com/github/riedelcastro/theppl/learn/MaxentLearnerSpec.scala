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
      case class LabelVar(data: Data) extends BoolVariable
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1, data2)
      class TestClassifier extends Classifier[Data] {
        type LabelType = Boolean
        type LabelVariableType = LabelVar
        def variable(context: Data) = LabelVar(context)
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context: Data) = vector(context.x)
      }
      val classifier = new TestClassifier
      val learner = new MaxentLearner[Data] with SupervisorByDeterministicExpectations[Data] {
        val module = classifier
        def expectator(model: module.ModelType) = Expectator(model)
        def instances = tokens
        def targetState(context: Data, model: module.ModelType) = model.labelVariable -> context.y
      }
      learner.train()

      val delta = new ParameterVector()

      for (i <- tokens; model = learner.module.model(i)) {
        val goldState = learner.targetState(i, model)
        val gold = model.features(goldState)
        val expectator = learner.expectator(model)
        val guess = expectator.expectations().featureExpectations
        delta.add(gold, 1.0)
        delta.add(guess, -1.0)
      }
      for ((_, value) <- delta.values)
        value must be(0.0 plusOrMinus epsLarge)

    }
  }

}