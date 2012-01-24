package com.github.riedelcastro.theppl

import java.util.UUID
import java.io.{InputStream, OutputStream}

/**
 * A module creates models based on a context and observation. Think of it as a parametrized
 * scoring function s_i(y_i;x_i) where i is the context, and x the observation.
 * The context is an object that (a) parametrizes the scoring function, and (b)
 * determines which hidden (c) determines which hidden and observed
 * variables the scoring function is defined over. For example, for
 * a PoS tagger the context could be a sentence object, and this would define
 * PoS tag variables as hidden and word form variables as observed variables.
 * A crucial difference to observations is that contexts can be arbitrary
 * application objects and don't have to be assignments to variables.
 * @author sriedel
 */
trait Module[-Context] {
  thisModule =>

  type ModelType <: Model

  /**
   * For a given context i this method creates
   * a model s_i(y).
   */
  def model(context: Context): ModelType

  /**
   * The name of this module.
   */
  val name: String = "Module(%s)".format(UUID.randomUUID.toString)

  /**
   * Returns the name of this module.
   */
  override def toString = name


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




