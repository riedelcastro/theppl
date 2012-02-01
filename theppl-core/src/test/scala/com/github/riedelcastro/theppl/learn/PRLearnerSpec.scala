package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._


/**
 * @author sriedel
 */
class PRLearnerSpec extends ThePPLSpec {

  describe("A PR Learner") {
    it("should match the posterior constraints under certain conditions") {
      case class Instance(x: Int)
      case class Var(instance: Instance) extends BoolVariable

      val trainingData = Range(0,10).map(Instance(_))

      val pModule = new Classifier[Instance] {
        type LabelType = Boolean
        type LabelVariableType = Var
        def labelFeatures(label: LabelType) = ParameterVector.fromFeats(Seq(Feat(label)))
        def contextFeatures(context: Instance) = ParameterVector.fromFeats(Seq(Feat(context.x)))
        def variable(context: Instance) = Var(context)
      }
      val qModule = new Classifier[Instance] {
        type LabelType = Boolean
        type LabelVariableType = Var
        def labelFeatures(label: LabelType) = ParameterVector.fromFeats(Seq(Feat(label)))
        def contextFeatures(context: Instance) = ParameterVector.fromFeats(Seq(Feat(true)))
        def variable(context: Instance) = Var(context)
      }

      val prLearner = new PRLearner[Instance] {
        val q = qModule
        val p = pModule
        def maxIterations = 4
        def targetExpectations(context: Instance, model: q.ModelType) = {
          val f = new ParameterVector()
          f(Feat(true,true)) = 0.7
          f(Feat(true,false)) = 0.3
          f
        }
        def instances = trainingData
      }

      prLearner.train()

      for (i <- trainingData) {
        val model = pModule.model(i)
        val expectations = Expectator(model).expectations()
        math.exp(expectations.logMarginals(model.labelVariable, true)) must be (0.7 plusOrMinus epsLarge)
      }

    }
  }

}