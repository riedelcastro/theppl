package com.github.riedelcastro.theppl

import collection.mutable.HashMap

/**
 * @author sriedel
 */
trait Expectator {

  def model: Model
  def expectations(penalties: Messages): Expectations

}

trait ExpectatorRecipe[-M <: Model] {
  def expectator(model: M, cookbook: ExpectatorRecipe[Model]): Expectator
}

object Expectator {
  def apply(model: Model, cookbook: ExpectatorRecipe[Model] = DefaultExpectator) = cookbook.expectator(model, cookbook)
}

object DefaultExpectator extends ExpectatorRecipe[Model] {
  def expectator(model: Model, cookbook: ExpectatorRecipe[Model]) = model match {
    case s: SumModel with LinearModel => SumProductBPRecipe.expectator(s,cookbook)
    case f: FiniteSupportModel with LinearModel => new BFExpectator {def model = f}
    case x => sys.error("Cannot do inference in " + x)
  }
}

trait BFExpectator extends Expectator {
  def model: FiniteSupportModel with LinearModel
  def expectations(penalties: Messages) = {

    val masses = new HashMap[(Variable[Any], Any), Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    val featExp = new HierarchicalParameterVector()
    var total = 0.0
    for (state <- model.allStates) {
      val mass = math.exp(model.penalizedScore(penalties, state))
      for (v <- model.hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
      featExp.add(model.features(state), mass)
    }
    featExp.scale(1.0 / total)
    new Expectations {
      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (x._2 / total)))
      lazy val logZ = math.log(total)
      lazy val featureExpectations = featExp
    }

  }


}