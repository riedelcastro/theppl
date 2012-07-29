package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import infer.Argmaxer

/**
 * The UpdateRule of an Online Learner defines how the prediction (guess) and
 * the gold data are used to update the weights.
 */
trait UpdateRule extends HasTemplate[Nothing] {
  def updateRule(potential: template.PotentialType, gold: State, guess: State)
}

trait UpdateRule2[C,P <: Potential] {
  type T = Template[C]{type PotentialType = P}
  def updateRule(template:T, potential: P, gold: State, guess: State)
}


trait OnlineLearner[Context] extends Learner[Context] {
  this:UpdateRule =>

  var epochs: Int = 2

  def argmaxer(potential:template.PotentialType):Argmaxer

  def train() {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val potential = template.potential(instance)
        val argmaxer = this.argmaxer(potential)
        val gold = potential.truth
        val guess = argmaxer.argmax().state
        updateRule(potential, gold, guess)
      }
    }
  }


}


trait HasTemplate2[Context,P <: Potential] {
  type T = Template[Context] {type PotentialType = P}
  def template:T
}





/**
 * The PerceptronUpdate simply adds the feature delta of gold and guess.
 */
trait PerceptronUpdate extends UpdateRule {

  val template:LinearTemplate[Nothing]
  var learningRate = 1.0

  def updateRule(potential: template.PotentialType, gold: State, guess: State) {
    val delta = potential.featureDelta(gold, guess)
    this.template.weights.add(delta, learningRate)
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
  this:Template[Nothing] =>
  def targetExpectations(potential:PotentialType):ParameterVector
}

trait ExpectationFromStateSupervisor extends ExpectationSupervisor with Supervisor {
  this:Template[Nothing] { type PotentialType <: LinearPotential } =>
  def targetExpectations(potential: PotentialType) = {
    potential.features(target(potential))
  }
}


trait SuperviseByState[Context] extends HasTemplate[Context] {
  def targetState(context:Context, potential:template.PotentialType):State
}
trait SuperviseByExpectations[Context] extends HasTemplate[Context] {
  def targetExpectations(context:Context, potential:template.PotentialType):ParameterVector
}

trait SuperviseByDeterministicExpectations[Context] extends SuperviseByExpectations[Context] with SuperviseByState[Context] {
  val template:LinearTemplate[Context]
  def targetExpectations(context:Context, potential:template.PotentialType):ParameterVector =
    potential.features(targetState(context,potential))
}


trait Learner[Context] extends HasTemplate[Context] {

  def instances:Seq[Context]
  def train()
}

trait Learner2[Context,P<:Potential] extends HasTemplate2[Context,P] {

  def instances:Seq[Context]
  def train()
}


