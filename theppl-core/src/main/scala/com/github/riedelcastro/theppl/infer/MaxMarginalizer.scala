package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.{Variable, Potential, Messages}
import scala.collection.mutable.HashMap
import com.github.riedelcastro.theppl.util.CollectionUtil

/**
 * @author Sebastian Riedel
 */
case class MaxMarginalizationResult(messages: Messages, max: Double) //todo: max should be objective?

trait MaxMarginalizer {
  def potential: Potential
  def maxMarginals(penalties: Messages = Messages.empty, variables: Iterable[Variable[Any]] = potential.hidden): MaxMarginalizationResult
}

object MaxMarginalizers {

  def exhaustive(pot: Potential): MaxMarginalizer = new MaxMarginalizer {
    def potential = pot
    def maxMarginals(penalties: Messages, variables: Iterable[Variable[Any]]) = {
      var max = Double.NegativeInfinity
      val scores = new HashMap[(Variable[Any], Any), Double]
      for (state <- CollectionUtil.allStatesIterator(pot.variables.toSeq)) {
        val score = pot.penalizedScore(penalties, state)
        for (v <- variables) scores(v -> state(v)) = math.max(scores.getOrElse(v -> state(v), Double.NegativeInfinity), score)
        if (score > max) max = score
      }
      MaxMarginalizationResult(Messages.fromMap(scores), max)
    }
  }

}



