package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._


/**
 * An OnlineLearner iterates over the training set and updates
 * weights after processing each instance.
 * @author sriedel
 */
trait OnlineLearner[-Context] extends LinearModule[Context] with Learner[Context] {
  self: UpdateRule =>

  var epochs: Int = 2

  def train(instances: Seq[Context]) {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val model = self.model(instance)
        val gold = target(model)
        val guess = model.predict
        updateRule(model, gold, guess)
      }
    }
  }

}

/**
 * The UpdateRule of an Online Learner defines how the prediction (guess) and
 * the gold data are used to update the weights.
 */
trait UpdateRule {
  this: OnlineLearner[Nothing] =>
  def updateRule(model: ModelType, gold: State, guess: State)
}

/**
 * The PerceptronUpdate simply adds the feature delta of gold and guess.
 */
trait PerceptronUpdate extends UpdateRule {
  this: OnlineLearner[Nothing] =>

  var learningRate = 1.0

  def updateRule(model: ModelType, gold: State, guess: State) {
    val delta = model.featureDelta(gold, guess)
    weights.add(delta, learningRate)
  }


}

trait Supervisor {
  this: Module[Nothing] =>
  def target(model: ModelType): State
}

trait ExpectationSupervisor {
  this:Module[Nothing] =>
  def targetExpectations(model:ModelType):HierarchicalParameterVector
}

trait ExpectationFromStateSupervisor extends ExpectationSupervisor with Supervisor {
  this:Module[Nothing] { type ModelType <: LinearModel } =>
  def targetExpectations(model: ModelType) = {
    model.features(target(model))
  }
}

trait Learner[-Context] extends Module[Context] with Supervisor {
  def train(instances: Seq[Context])
}
