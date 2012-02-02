package com.github.riedelcastro.theppl

import logic.Term

/**
 * A Model is a scoring function s(Y=y) over a set of variables Y.
 */
trait Model extends Term[Double] {
  thisModel =>

  /**
   * The set of hidden variables
   */
  def hidden: Iterable[Variable[Any]]

  /**
   * Returns a score s(y) for each assignment to the hidden variables of this model.
   */
  def score(state: State): Double

  /**
   * Convenience method to score a state with added penalties.
   */
  def penalizedScore(penalties: Messages, state: State) = {
    score(state) + hidden.map(v => penalties.msg(v, state(v))).sum
  }

  /**
   * Each model comes with a way to create a default marginalizer. This marginalizer
   * may be composed of other marginalizers for sub-modules, and hence takes a
   * cookbook as argument. By default marginalization is done in brute-force fashion.
   */
  def defaultMarginalizer(cookbook: MarginalizerRecipe[Model]): Marginalizer = new BFMarginalizer {val model = thisModel}

  /**
   * A model also has some default way of finding its argmax state.
   */
  def defaultArgmaxer(cookbook: ArgmaxRecipe[Model]): Argmaxer = new BruteForceArgmaxer {val model = thisModel}

  /**
   * A model evaluates to its score.
   */
  def eval(state: State) = Some(score(state))


  def variables = hidden


}

object Model {
  val zero = new Model {
    def hidden = Seq.empty
    def score(state: State) = 0.0
  }
}

/**
 * An object that has a model it can use for computation of all sorts. One purpose of
 * this trait is to allow clients to use type members of modules without having
 * to mix-in the module trait. Inference algorithms use this trait
 * to decouple inference object from model object.
 */
trait HasModel {
  type ModelType = model.type
  val model: Model
}



