package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import Imports._
import infer.Expectator


/**
 * @author sriedel
 */
class MaxentLearnerSpec extends ThePPLSpec {

  describe("A MaxentLearner") {
    it("should yield a potential that has the same expectations as the training data") {
      case class Data(x: Int, y: Boolean)
      case class LabelVar(data: Data) extends BoolVariable
      val data1 = Data(0, true)
      val data2 = Data(1, false)
      val tokens = Seq(data1, data2)
      class TestClassifier extends Classifier[Data] {
        type LabelType = Boolean
        type LabelVariableType = LabelVar
        def variable(context: Data) = LabelVar(context)
        def labelFeatures(label: LabelType) = ParameterVector(label)
        def contextFeatures(context: Data) = ParameterVector(context.x)
      }
      val classifier = new TestClassifier
      val learner = new MaxentLearner[Data] with SupervisorByDeterministicExpectations[Data] {
        val template = classifier
        def expectator(potential: template.PotentialType) = Expectator(potential)
        def instances = tokens
        def targetState(context: Data, potential: template.PotentialType) = potential.labelVariable -> context.y
      }
      learner.train()

      val delta = new ParameterVector()

      for (i <- tokens; potential = learner.template.potential(i)) {
        val goldState = learner.targetState(i, potential)
        val gold = potential.features(goldState)
        val expectator = learner.expectator(potential)
        val guess = expectator.expectations().featureExpectations
        delta.add(gold, 1.0)
        delta.add(guess, -1.0)
      }
      for ((_, value) <- delta.values)
        value must be(0.0 plusOrMinus epsLarge)

    }
  }

}