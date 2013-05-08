package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.util.HasLogger
import com.github.riedelcastro.theppl._

/**
 * An object that can calculate the argmax state for a given potential.
 * @author sriedel
 */
trait Argmaxer{

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

/**
 * Argmaxer that finds the assignment with minimum Bayes Risk by
 * choosing, for each variable, the assignment with maximum marginal probability. This requires
 * an underlying marginalizer.
 */
trait MinimumBayesRiskArgmaxer extends Argmaxer with HasPotential {

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


/**
 * Finds the argmax by explicitly searching through all possible assignments to the hidden variables
 * of a potential.
 */
trait BruteForceArgmaxer extends Argmaxer with HasPotential with HasLogger {

  def argmax(penalties: Messages) = {
    logger.trace("Bruteforce argmaxer used")
    val states = potential.allStates
    val scored = states.map(state => Scored(state, potential.penalizedScore(penalties, state)))
    val result = scored.maxBy(_.score)
    new ArgmaxResult {
      def state = result.value
      def score = result.score
    }
  }

}

/**
 * Creates argmaxers which maximize each factor in isolation.
 */
object NaiveFactoredArgmaxerRecipe extends ArgmaxRecipe[SumPotential] {
  def argmaxer(pot: SumPotential, cookbook: ArgmaxRecipe[Potential]) = {
    new Argmaxer {
      val potential = pot
      val argmaxers = pot.args.map(cookbook.argmaxer(_, cookbook))
      def argmax(penalties: Messages) = {
        var result: State = State.empty
        for (argmaxer <- argmaxers) result = result + argmaxer.argmax(penalties).state
        new ArgmaxResult {
          def state = result
          lazy val score = pot.score(state)
        }
      }
    }
  }
}

