package com.github.riedelcastro.theppl.infer

import collection.mutable.HashMap
import math._
import com.github.riedelcastro.theppl._
import util.HasLogger
import collection.mutable

/**
 * @author sriedel
 */
trait Expectator extends Marginalizer {

  def expectations(penalties: Messages = Messages.empty): Expectations
  def marginalize(penalties: Messages): MarginalizeResult = expectations(penalties)
}

trait Marginalizer extends HasPotential {

  def marginalize(penalties: Messages = Messages.empty): MarginalizeResult

}

trait MarginalizerRecipe[-M <: Potential] {
  def marginalizer(potential: M, cookbook: MarginalizerRecipe[Potential]): Marginalizer
}

object Marginalizer extends MarginalizerRecipe[Potential] {
  def marginalizer(potential: Potential, cookbook: MarginalizerRecipe[Potential]) = potential.defaultMarginalizer(cookbook)
  def apply(potential: Potential, cookbook: MarginalizerRecipe[Potential] = this) = marginalizer(potential, cookbook)
}

/**
 * The result of an marginalize call.
 */
trait MarginalizeResult {
  def logMarginals: Messages
  def logZ: Double
}


/**
 * The result of marginalizing a linear potential also contains expectations of the features/sufficient statistics.
 */
trait Expectations extends MarginalizeResult {
  def featureExpectations: ParameterVector
}


trait ExpectatorRecipe[-M <: Potential] {
  def expectator(potential: M, cookbook: ExpectatorRecipe[Potential] = Expectator): Expectator
}

object Expectator extends ExpectatorRecipe[Potential] {


  def expectator(potential: Potential, cookbook: ExpectatorRecipe[Potential]) = potential match {
    case f: FeaturePotential => f.defaultExpectator(cookbook)
    case m => sys.error("Cannot do inference in " + m)
  }
  def apply(potential: Potential, cookbook: ExpectatorRecipe[Potential] = this) = cookbook.expectator(potential, cookbook)
}


object BruteForceExpectator extends ExpectatorRecipe[FeaturePotential] {
  def expectator(fm: FeaturePotential, cookbook: ExpectatorRecipe[Potential]) = new BFExpectator {
    val potential = fm
  }
}
trait BFMarginalizer extends Marginalizer {

  val potential: Potential

  def marginalize(penalties: Messages) = {
    val masses = new HashMap[(Variable[Any], Any), Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    var total = 0.0
    for (state <- potential.allStates) {
      val mass = math.exp(potential.penalizedScore(penalties, state))
      for (v <- potential.hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
    }
    new MarginalizeResult {
      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (log(x._2) - log(total))))
      lazy val logZ = math.log(total)
    }
  }
}

trait BFExpectator extends Expectator with HasLogger {
  val potential: FeaturePotential
  def expectations(penalties: Messages) = {
    logger.trace("Bruteforce expectator used")
    val masses = new mutable.HashMap[(Variable[Any], Any), Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    val featExp = new ParameterVector()
    var total = 0.0
    for (state <- potential.allStates) {
      val mass = math.exp(potential.penalizedScore(penalties, state))
      for (v <- potential.hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
      featExp.add(potential.features(state), mass)
    }
    featExp.scale(1.0 / total)
    new Expectations {
      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (log(x._2) - log(total))))
      lazy val logZ = math.log(total)
      lazy val featureExpectations = featExp
    }

  }


}