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

  def variables = hidden
  def eval(state: State) = Some(score(state))
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



