package com.github.riedelcastro.theppl

import util.StreamUtil
import collection.mutable.HashMap


/**
 * A Model is a scoring function s(Y=y) over a set of variables Y.
 */
trait FiniteSupportModel extends Model {

  /**
   * Iterates over all states for the hidden variables of this model.
   */
  def allStates = {
    val variables = hidden.toSeq
    val domains = variables.map(_.domain).toSeq
    val tuples = StreamUtil.allTuples(domains)
    val states = tuples.map(State(variables, _))
    states
  }
}


/**
 * This model does argmax by exhaustively iterating over all states.
 * Generally very slow, and only recommended to use for testing other methods.
 */
trait BruteForceArgmaxer extends FiniteSupportModel {

  def argmax(penalties: Message) = {
    val states = allStates
    def scored = states.map(state => Scored(state, penalizedScore(penalties, state)))
    val result = scored.maxBy(_.score)
    new ArgmaxResult {
      def state = result.value
      def score = result.score
    }
  }
}

/**
 * Marginalizes model by exhaustively iterating over all states.
 */
trait BruteForceMarginalizer extends FiniteSupportModel {
  def marginalize(penalties: Message) = {
    val masses = new HashMap[(Variable[Any],Any),Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    var total = 0.0
    for (state <- allStates) {
      val mass = math.exp(penalizedScore(penalties,state))
      for (v <- hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
    }
    new MarginalizeResult {
      lazy val marginals = Message.fromMap(masses.map(x => x._1 -> (x._2 /total)))
      lazy val logZ = math.log(total)
    }
    
  }
}





