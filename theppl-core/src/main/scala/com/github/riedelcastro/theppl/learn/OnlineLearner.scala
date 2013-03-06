package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import infer.Argmaxer
import org.riedelcastro.nurupo.ProgressMonitor

/**
 * The UpdateRule of an Online Learner defines how the prediction (guess) and
 * the gold data are used to update the weights.
 */
trait UpdateRule extends HasTemplate[Nothing] {
  def updateRule(potential: template.PotentialType, gold: State, guess: State)
}


trait OnlineLearner[Context] extends Learner[Context] {
  this: UpdateRule =>

  var epochs: Int = 2

  def argmaxer(potential: template.PotentialType): Argmaxer = potential.defaultArgmaxer()

  def train() {
    ProgressMonitor.start(this, "OnlineLearner", instances.size, epochs)
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val potential = template.potential(instance)
        val argmaxer = this.argmaxer(potential)
        val gold = potential.truth
        val guess = argmaxer.argmax().state
        updateRule(potential, gold, guess)
        ProgressMonitor.progress(this)
      }
    }
  }

}


/**
 * The PerceptronUpdate simply adds the feature delta of gold and guess.
 */
trait PerceptronUpdate extends UpdateRule {

  val template: LinearTemplate[Nothing]
  var learningRate = 1.0

  def featureDelta(potential: template.PotentialType, gold: State, guess: State) = {
    potential.featureDelta(gold, guess)
  }

  def updateRule(potential: template.PotentialType, gold: State, guess: State) {
    val delta = featureDelta(potential, gold, guess)
    template.weights.add(delta, learningRate)
  }

}

//trait PerceptronUpdate2[Context] extends UpdateRule2[Context,LinearPotential] {
//
//  val template:LinearTemplate[Nothing]
//  var learningRate = 1.0
//
//  def updateRule(template:T, potential: LinearPotential, gold: State, guess: State) {
//    val delta = potential.featureDelta(gold, guess)
//    template.weights.add(delta, learningRate)
//  }
//
//
//}


trait Supervisor {
  this: Template[Nothing] =>
  def target(potential: PotentialType): State
}

trait ExpectationSupervisor {
  this: Template[Nothing] =>
  def targetExpectations(potential: PotentialType): ParameterVector
}

trait ExpectationFromStateSupervisor extends ExpectationSupervisor with Supervisor {
  this: Template[Nothing] {type PotentialType <: LinearPotential} =>
  def targetExpectations(potential: PotentialType) = {
    potential.features(target(potential))
  }
}


trait SuperviseByState[Context] extends HasTemplate[Context] {
  def targetState(context: Context, potential: template.PotentialType): State
}

trait SuperviseByExpectations[Context] extends HasTemplate[Context] {
  def targetExpectations(context: Context, potential: template.PotentialType): ParameterVector
}

trait SuperviseByDeterministicExpectations[Context] extends SuperviseByExpectations[Context] with SuperviseByState[Context] {
  val template: LinearTemplate[Context]
  def targetExpectations(context: Context, potential: template.PotentialType): ParameterVector =
    potential.features(targetState(context, potential))
}


trait Learner[Context] extends HasTemplate[Context] {

  def instances: Seq[Context]
  def train()
}

trait Learner2 {
  def train[Context, T <: Template[Context]](template:T)(instances:Seq[template.C])
}


