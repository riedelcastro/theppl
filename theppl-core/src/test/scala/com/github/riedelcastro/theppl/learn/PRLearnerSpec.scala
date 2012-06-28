package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import infer.Expectator


/**
 * @author sriedel
 */
class PRLearnerSpec extends ThePPLSpec {

  describe("A PR Learner") {
    it("should match the posterior constraints under certain conditions") {
      case class Instance(x: Int)
      case class Var(instance: Instance) extends BoolVariable

      val trainingData = Range(0,10).map(Instance(_))

      val pTemplate = new Classifier[Instance] {
        type LabelType = Boolean
        type LabelVariableType = Var
        def labelFeatures(label: LabelType) = ParameterVector(label)
        def contextFeatures(context: Instance) = ParameterVector(context.x)
        def variable(context: Instance) = Var(context)
      }
      val qTemplate = new Classifier[Instance] {
        type LabelType = Boolean
        type LabelVariableType = Var
        def labelFeatures(label: LabelType) = ParameterVector(label)
        def contextFeatures(context: Instance) = ParameterVector(true)
        def variable(context: Instance) = Var(context)
      }

      val prLearner = new PRLearner[Instance] {
        val q = qTemplate
        val p = pTemplate
        def maxIterations = 5
        override def maxQIterations = 5
        override def maxPIterations = 5
        def targetExpectations(context: Instance, potential: q.PotentialType) = {
          val f = new ParameterVector()
          f(IndexedSeq(true,true)) = 0.7
          f(IndexedSeq(true,false)) = 0.3
          f
        }
        def instances = trainingData
      }

      prLearner.train()

      for (i <- trainingData) {
        val potential = pTemplate.potential(i)
        val expectations = Expectator(potential).expectations()
        math.exp(expectations.logMarginals(potential.labelVariable, true)) must be (0.7 plusOrMinus epsLarge)
      }

    }
  }

}