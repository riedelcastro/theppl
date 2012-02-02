package com.github.riedelcastro.theppl

import util.StreamUtil
import collection.mutable.HashMap
import math._


/**
 * A Model is a scoring function s(Y=y) over a set of variables Y.
 */
//trait FiniteSupportModel extends Model {
//  self =>
//
//  /**
//   * Iterates over all states for the hidden variables of this model.
//   */
//  def allStates = {
//    val variables = hidden.toSeq
//    val domains = variables.map(_.domain).toSeq
//    val tuples = StreamUtil.allTuples(domains)
//    val states = tuples.map(State(variables, _))
//    states
//  }
//  override def defaultMarginalizer(cookbook: MarginalizerRecipe[Model]) = new BFMarginalizer {val model = self}
//  override def defaultArgmaxer(cookbook: ArgmaxRecipe[Model]) = new BruteForceArgmaxer {val model = self}
//
//}

/**
 * Marginalizes model by exhaustively iterating over all states.
 */
//trait BruteForceMarginalizer extends FiniteSupportModel {
//  def marginalize(penalties: Messages) = {
//    val masses = new HashMap[(Variable[Any], Any), Double] {
//      override def default(key: (Variable[Any], Any)) = 0.0
//    }
//    var total = 0.0
//    for (state <- allStates) {
//      val mass = math.exp(penalizedScore(penalties, state))
//      for (v <- hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
//      total += mass
//    }
//    new MarginalizeResult {
//      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (log(x._2) - log(total))))
//      lazy val logZ = math.log(total)
//    }
//
//  }
//}





