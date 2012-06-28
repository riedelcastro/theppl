package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.util.HasLogger
import com.github.riedelcastro.theppl._

/**
 * An object that can calculate the argmax state for a given potential.
 * @author sriedel
 */
trait Argmaxer extends HasPotential {

  def argmax(penalties: Messages = Messages.empty): ArgmaxResult
  def predict = argmax(Messages.empty).state

}

/**
 * The result of an argmax call. Has the argmaxing state and its score.
 */
trait ArgmaxResult {
  def state: State
  def score: Double
}


trait ArgmaxRecipe[-PotentialType <: Potential] {
  def argmaxer(potential: PotentialType, cookbook: ArgmaxRecipe[Potential] = Argmaxer): Argmaxer
}

object Argmaxer extends ArgmaxRecipe[Potential] {

  def argmaxer(potential: Potential, cookbook: ArgmaxRecipe[Potential]) = potential.defaultArgmaxer(cookbook)
  def apply(potential: Potential, cookbook: ArgmaxRecipe[Potential] = this) =
    cookbook.argmaxer(potential, cookbook)
}

trait MinimumBayesRiskArgmaxer extends Argmaxer {

  def marginalizer: Marginalizer
  def threshold: Double

  def argmax(penalties: Messages) = {
    val marginals = marginalizer.marginalize(penalties).logMarginals
    new ArgmaxResult {
      lazy val score = potential.score(state)
      lazy val state = new State {
        def get[V](variable: Variable[V]) =
          Some(marginals.message(variable).map(math.exp(_)).offsetDefault(-threshold).argmax)
      }
    }
  }
}

trait BruteForceArgmaxer extends Argmaxer with HasLogger {

  val potential: Potential

  def argmax(penalties: Messages) = {
    logger.trace("Bruteforce argmaxer used")
    val states = potential.allStates
    def scored = states.map(state => Scored(state, potential.penalizedScore(penalties, state)))
    val result = scored.maxBy(_.score)
    new ArgmaxResult {
      def state = result.value
      def score = result.score
    }
  }


}