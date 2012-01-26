package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._

/**
 * The UpdateRule of an Online Learner defines how the prediction (guess) and
 * the gold data are used to update the weights.
 */
trait UpdateRule extends HasModule[Nothing] {
  def updateRule(model: module.ModelType, gold: State, guess: State)
}

trait OnlineLearner[Context] extends Learner[Context] with SuperviseByState[Context] {
  this:UpdateRule =>

  var epochs: Int = 2

  def argmaxer(model:module.ModelType):Argmaxer

  def train() {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val model = module.model(instance)
        val argmaxer = this.argmaxer(model)
        val gold = targetState(instance, model)
        val guess = argmaxer.argmax().state
        updateRule(model, gold, guess)
      }
    }
  }


}



/**
 * The PerceptronUpdate simply adds the feature delta of gold and guess.
 */
trait PerceptronUpdate extends UpdateRule {

  val module:LinearModule[Nothing]
  var learningRate = 1.0

  def updateRule(model: module.ModelType, gold: State, guess: State) {
    val delta = model.featureDelta(gold, guess)
    this.module.weights.add(delta, learningRate)
  }


}


trait Supervisor {
  this: Module[Nothing] =>
  def target(model: ModelType): State
}

trait ExpectationSupervisor {
  this:Module[Nothing] =>
  def targetExpectations(model:ModelType):ParameterVector
}

trait ExpectationFromStateSupervisor extends ExpectationSupervisor with Supervisor {
  this:Module[Nothing] { type ModelType <: LinearModel } =>
  def targetExpectations(model: ModelType) = {
    model.features(target(model))
  }
}


trait SuperviseByState[Context] extends HasModule[Context] {
  def targetState(context:Context,  model:module.ModelType):State
}
trait SuperviseByExpectations[Context] extends HasModule[Context] {
  def targetExpectations(context:Context,  model:module.ModelType):ParameterVector
}

trait SupervisorByDeterministicExpectations[Context] extends SuperviseByExpectations[Context] with SuperviseByState[Context] {
  val module:LinearModule[Context]
  def targetExpectations(context:Context,  model:module.ModelType):ParameterVector = 
    model.features(targetState(context,model))
}


trait Learner[Context] extends HasModule[Context] {

  def instances:Seq[Context]
  def train()
}

