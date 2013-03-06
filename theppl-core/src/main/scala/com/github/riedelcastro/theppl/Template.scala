package com.github.riedelcastro.theppl

import infer.{ArgmaxResult, BruteForceArgmaxer, Argmaxer, ArgmaxRecipe}
import java.io.{InputStream, OutputStream}
import logic.Term

/**
 * A template creates potentials based on a context and observation. Think of it as a parametrized
 * scoring function s_i(y_i) where i is a context parameter.
 * The context is an object that (a) parametrizes the scoring function, and (b)
 * determines which hidden (c) determines which hidden and observed
 * variables the scoring function is defined over. For example, for
 * a PoS tagger the context could be a sentence object, and this would define
 * PoS tag variables as hidden and word form variables as observed variables.
 * A crucial difference to observations is that contexts can be arbitrary
 * application objects and don't have to be assignments to variables.
 * @author sriedel
 */
trait Template[-Context] extends Term[Context => Double] {
  thisTemplate =>

  type C = Context

  /**
   * The type of potential this template creates.
   */
  type PotentialType <: Potential

  /**
   * For a given context i this method creates
   * a potential s_i(y).
   */
  def potential(context: Context): PotentialType

  /**
   * A template is also a term that evaluates to a function from context to scores.
   */
  def eval(state: State) = Some((c: Context) => potential(c).score(state))

  /**
   * A template can potentially contain infinite amounts of variables, one set for every possible context. Hence
   * this method is generally not well defined.
   */
  def variables = sys.error("Potentially infinite number of variables")

  /**
   * Convenience method that returns the argmax for the potential corresponding to the
   * given context and input penalties.
   */
  def argmax(context: Context, penalties: Messages = Messages.empty)
            (implicit cookbook: ArgmaxRecipe[Potential] = Argmaxer): ArgmaxResult = {
    cookbook.argmaxer(potential(context)).argmax(penalties)
  }

  /**
   * Convenience method that returns state corresponding to argmax solution.
   */
  def predict(context: Context, penalties: Messages = Messages.empty)
             (implicit cookbook: ArgmaxRecipe[Potential] = Argmaxer) = argmax(context, penalties)(cookbook).state


}

object EmptyTemplate extends Template[Any] {
  type PotentialType = Nothing
  def potential(context: Any) = sys.error("Template empty")
}


/**
 * An object that has a template it can use for computation of all sorts. One purpose of
 * this trait is to allow clients to use type members of templates without having
 * to mix-in the template trait. Learners and evaluators
 * all use this trait.
 */
trait HasTemplate[-Context] {
  type PotentialType = template.PotentialType
  type TemplateType = template.type
  val template: Template[Context]
}

/**
 * A template that can be stored to and loaded from output and input streams, respectively.
 */
trait SerializableTemplate[Context] extends Template[Context] {
  /**
   * Serialize this template.
   */
  def save(out: OutputStream)

  /**
   * Deserialize this template.
   */
  def load(in: InputStream)

}




