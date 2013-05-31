package com.github.riedelcastro.theppl

import infer._
import term.Term
import com.github.riedelcastro.theppl.util.CollectionUtil

/**
 * A Potential is a scoring function s(Y=y) over a set of variables Y.
 */
trait Potential extends Term[Double] {
  thisPotential =>

  /**
   * The set of hidden variables
   */
  def hidden: Iterable[Variable[Any]]

  /**
   * Returns a score s(y) for each assignment to the hidden variables of this potential.
   */
  def score(state: State): Double

  /**
   * Convenience method to score a state with added penalties.
   */
  def penalizedScore(penalties: Messages, state: State) = {
    score(state) + hidden.map(v => penalties.msg(v, state(v))).sum
  }

  /**
   * Each potential comes with a way to create a default marginalizer. This marginalizer
   * may be composed of other marginalizers for sub-templates, and hence takes a
   * cookbook as argument. By default marginalization is done in brute-force fashion.
   */
  def defaultMarginalizer(cookbook: MarginalizerRecipe[Potential] = Marginalizer): Marginalizer = new BFMarginalizer {
    val potential = thisPotential
  }

  /**
   * A potential also has some default way of finding its argmax state.
   */
  def defaultArgmaxer(cookbook: ArgmaxRecipe[Potential] = Argmaxer): Argmaxer = new BruteForceArgmaxer {
    val potential = thisPotential
  }

  /**
   * Convenience method that uses the default marginalizer.
   */
  def marginalize(penalties: Messages = Messages.empty) = defaultMarginalizer().marginalize(penalties)

  /**
   * Convenience method that uses the default argmaxer.
   */
  def argmax(penalties: Messages = Messages.empty) = defaultArgmaxer().argmax(penalties)

  /**
   * A potential evaluates to its score.
   */
  def eval(state: State) = Some(score(state))


  /**
   * Implementing Term functionality.
   * @return the hidden variables of this potential.
   */
  def variables = hidden

  /**
   * The potential may know what the "true vales" of its variables are. This can be used
   * for training the potential. By default this true state is empty.
   * @return a state representing the "true assignment" of the potential's variables. For every
   *         variable for which no truth is known, the state has an empty/null assignment.
   */
  def truth: State = State.empty

  def default = 0.0
}


/**
 * A potential that uses a mapping from tuples to scores for its scoring function.
 * @param hidden the hidden variables.
 * @param scores the scores assigned to assignments of hidden variables. The order of each tuple has to correspond
 *               to the order of the hidden variables.
 * @param default the default score to return if no matching tuple can be found.
 */
case class TablePotential(hidden: Seq[Variable[Any]], scores: Map[Seq[Any], Double], override val default: Double = 0.0)
  extends Potential {

  def score(state: State) = scores.getOrElse(hidden.view.map(v => state(v)), default)
}

/**
 * Slightly more convenient version
 */
case class Table(hidden: Seq[Variable[Any]], scores: Seq[Any] => Double, override val default: Double = 0.0)
  extends Potential {
  val scoreMap = CollectionUtil.allTuples(hidden.map(_.domain)).map(t => t -> scores(t)).toMap
  def score(state: State) = scoreMap.getOrElse(hidden.view.map(v => state(v)), default)
  override def toString =
    hidden.mkString(",") + "\n" +
    CollectionUtil.allTuplesIterator(hidden.map(_.domain)).map(
    t => "%20s %8.4f".format(t.mkString(","),scores(t))).mkString("\n")
}


object Potential {

  def table(hidden: Seq[Variable[Any]], scores: Seq[Any] => Double) = Table(hidden, scores)

  def sum(sumArgs: Seq[Potential]) = new SumPotential {
    def args = sumArgs
  }

  /**
   * Convenience method to score a state with added penalties.
   */
  def penalizedScore(potential: Term[Double], penalties: Messages, state: State) = {
    potential.eval(state).get + potential.variables.map(v => penalties.msg(v, state(v))).sum
  }


}

/**
 * An object that has a potential it can use for computation of all sorts. One purpose of
 * this trait is to allow clients to use type members of templates without having
 * to mix-in the template trait. Inference algorithms use this trait
 * to decouple inference object from potential object.
 */
trait HasPotential {
  type PotentialType = potential.type
  val potential: Potential
}



