package com.github.riedelcastro.theppl

import collection.mutable.HashMap
import math._

/**
 * @author sriedel
 */
trait Expectator extends Marginalizer {

  val model:Model

  def expectations(penalties: Messages = Messages.empty): Expectations
  def marginalize(penalties: Messages): MarginalizeResult = expectations(penalties)
}

trait Marginalizer extends HasModel {

  def marginalize(penalties: Messages = Messages.empty): MarginalizeResult

}

trait MarginalizerRecipe[-M <: Model] {
  def marginalizer(model: M, cookbook: MarginalizerRecipe[Model]): Marginalizer
}

object Marginalizer extends MarginalizerRecipe[Model] {
  def marginalizer(model: Model, cookbook: MarginalizerRecipe[Model]) = model.defaultMarginalizer(cookbook)
  def apply(model:Model, cookbook:MarginalizerRecipe[Model] = this) = marginalizer(model, cookbook)
}

/**
 * The result of an marginalize call.
 */
trait MarginalizeResult {
  def logMarginals: Messages
  def logZ: Double
}


/**
 * The result of marginalizing a linear model also contains expectations of the features/sufficient statistics.
 */
trait Expectations extends MarginalizeResult {
  def featureExpectations: ParameterVector
}


trait ExpectatorRecipe[-M <: Model] {
  def expectator(model: M, cookbook: ExpectatorRecipe[Model] = Expectator): Expectator
}

object Expectator extends ExpectatorRecipe[Model]  {


  def expectator(model: Model, cookbook: ExpectatorRecipe[Model]) = model match {
    case f:FeatureModel => f.defaultExpectator(cookbook)
    case m => sys.error("Cannot do inference in " + m)
  }
  def apply(model: Model, cookbook: ExpectatorRecipe[Model] = this) = cookbook.expectator(model, cookbook)
}


object BruteForceExpectator extends ExpectatorRecipe[FeatureModel] {
  def expectator(fm: FeatureModel, cookbook: ExpectatorRecipe[Model]) = new BFExpectator {
    val model = fm
  }
}
trait BFMarginalizer extends Marginalizer {

  val model: Model

  def marginalize(penalties: Messages) = {
    val masses = new HashMap[(Variable[Any], Any), Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    var total = 0.0
    for (state <- model.allStates) {
      val mass = math.exp(model.penalizedScore(penalties, state))
      for (v <- model.hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
    }
    new MarginalizeResult {
      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (log(x._2) - log(total))))
      lazy val logZ = math.log(total)
    }
  }
}

trait BFExpectator extends Expectator {
  val model: FeatureModel
  def expectations(penalties: Messages) = {

    val masses = new HashMap[(Variable[Any], Any), Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    val featExp = new ParameterVector()
    var total = 0.0
    for (state <- model.allStates) {
      val mass = math.exp(model.penalizedScore(penalties, state))
      for (v <- model.hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
      featExp.add(model.features(state), mass)
    }
    featExp.scale(1.0 / total)
    new Expectations {
      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (log(x._2) - log(total))))
      lazy val logZ = math.log(total)
      lazy val featureExpectations = featExp
    }

  }


}