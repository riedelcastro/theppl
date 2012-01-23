package com.github.riedelcastro.theppl

import logic.{Bool, Substitution, Term}


/**
 * A Variable is an identifier we can assign values to. Notably,
 * a value of variable is not stored in the variable itself. Instead
 * it is stored in State objects that map variables to values. This
 * allows several beliefs of the state of a variable to exist in the same
 * time.
 *
 * Every Variable defines the class of scala objects its values can have.
 *
 * @author sriedel
 */
trait Variable[+V] extends Term[V] {
  /**
   * This returns the value of a variable with respect to a specified state.
   * Since the state is an implicit parameter, this method allows
   * clients to use variables as if they have a single value, if needed.
   */
  def eval(state: State) = state.get(this)
  def variables = Seq(this)
  def domain: Seq[V]
  def default = domain.head
  override def substitute(substitution: Substitution) = substitution.get(this).getOrElse(this)
}

trait BoolVariable extends Variable[Boolean] {
  def domain = Bool.values
}


