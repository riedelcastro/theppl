package com.github.riedelcastro.theppl

import term.{Bool, Substitution, Term}
import util.Util


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
  def stringRepr = "X"
  override def substitute(substitution: Substitution) = substitution.get(this).getOrElse(this)
}

/**
 * A variable that represents the target state of another variable.
 * @param variable the variable for which this variable represents the target state.
 * @tparam V the type of the variable.
 */
case class Target[+V](variable:Variable[V]) extends Variable[V] {
  def domain = variable.domain
  override def default = variable.default
}


trait BoolVariable extends Variable[Boolean] {
  def domain = Bool.values
}

case class BoolVar[Id](id:Id) extends BoolVariable

case class IntVar[Id](id:Id) extends Variable[Int] {
  def domain = Util.infinity
  override def default = 0
  override def stringRepr = id.toString.replaceAll("'","")
}

case class VectorVar[Id](id:Id) extends Variable[ParameterVector] {
  def domain = Util.infinity
  override def default = new ParameterVector
}
