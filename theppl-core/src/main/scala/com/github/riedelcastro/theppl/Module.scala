package com.github.riedelcastro.theppl

import java.util.UUID
import java.io.{InputStream, OutputStream}
import logic.Term

/**
 * A module creates models based on a context and observation. Think of it as a parametrized
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
trait Module[-Context] extends Term[Context => Double] {
  thisModule =>

  type ModelType <: Model

  /**
   * For a given context i this method creates
   * a model s_i(y).
   */
  def model(context: Context): ModelType

  /**
   * A module is also a term that evaluates to a function from context to scores.
   */
  def eval(state: State) = Some((c: Context) => model(c).score(state))

  /**
   * A module can potentially contain infinite amounts of variables, one set for every possible context. Hence
   * this method is generally not well defined.
   */
  def variables = sys.error("Potentially infinite number of variables")
}

/**
 * A module that can be stored to and loaded from output and input streams, respectively.
 */
trait SerializableModule[Context] extends Module[Context] {
  /**
   * Serialize this module.
   */
  def save(out: OutputStream)

  /**
   * Deserialize this module.
   */
  def load(in: InputStream)

}




