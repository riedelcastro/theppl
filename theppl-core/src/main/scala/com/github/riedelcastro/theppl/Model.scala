package com.github.riedelcastro.theppl

import logic.Term


/**
 * A Model is a scoring function s(Y=y) over a set of variables Y.
 */
trait Model extends Term[Double]{
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
   * Returns the assignment to hidden variables of this model that maximizes the score,
   * with added penalties on the variables.
   */
  def argmax(penalties: Messages): ArgmaxResult

  /**
   * Convenience method for when no incoming message is needed.
   */
  def predict: State = argmax(Messages.empty).state

  /**
   * Convenience method to score a state with added penalties.
   */
  def penalizedScore(penalties: Messages, state: State) = {
    score(state) + hidden.map(v => penalties.msg(v, state(v))).sum
  }

  /**
   * Proxy class for decoration.
   */
  class decorated extends Proxy.Typed[Model] with Model {
    def hidden = thisModel.hidden
    def score(state: State) = thisModel.score(state)
    def argmax(penalties: Messages) = thisModel.argmax(penalties)
    def self = thisModel
  }

  def variables = hidden
  def eval(state: State) = Some(score(state))
}

/**
 * The result of an argmax call. Has the argmaxing state and its score.
 */
trait ArgmaxResult {
  def state: State
  def score: Double
}

/**
 * The result of an marginalize call.
 */
trait MarginalizeResult {
  def logMarginals: Messages
  def logZ: Double
}


