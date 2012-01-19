package com.github.riedelcastro.theppl

import util.StreamUtil


/**
 * A Model is a scoring function s(Y=y) over a set of variables Y.
 */
trait Model {
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
  def argmax(penalties: Message): State

  /**
   * Convenience method for when no incoming message is needed.
   */
  def predict: State = argmax(Message.emtpy)

  /**
   * Proxy class for decoration.
   */
  class decorated extends Proxy.Typed[Model] with Model {
    def hidden = thisModel.hidden
    def score(state: State) = thisModel.score(state)
    def argmax(penalties: Message) = thisModel.argmax(penalties)
    def self = thisModel
  }
}

/**
 * A model for which the set of states with
 * score > NEGINF is finite. In this case we can
 * perform inference automatically (although
 * generally not very efficiently).
 */
trait FiniteSupportModel extends Model {

  /**
   * The variables of this model together with their supported range.
   */
  def restrictions: Iterable[Restriction[Any]]

  def hidden = restrictions.map(_.variable)
}

/**
 * This model does inference by exhaustively iterating over all states.
 * Generally very slow!
 */
trait BruteForceModel extends FiniteSupportModel {
  def argmax(penalties: Message) = {
    val variables = restrictions.map(_.variable).toSeq
    val domains = restrictions.map(_.domain).toSeq
    val tuples = StreamUtil.allTuples(domains)
    val states = tuples.map(State(variables,_))
    def penalizedScore(state:State) = {
      score(state) + variables.map(v => penalties.msg(v,state(v))).sum
    }
    def scored = states.map(state => Scored(state,penalizedScore(state)))
    scored.maxBy(_.score).value
  }
}



