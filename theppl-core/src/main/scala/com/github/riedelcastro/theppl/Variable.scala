package com.github.riedelcastro.theppl

/**
 * A Variable is an identifier we can assign values to. Notably,
 * a value of variable is not stored in the variable itself. Instead
 * it is stored in State objects that map variables to values. This
 * allows several beliefs of the state of a variable to exist in the same
 * time.
 *
 * Every Variable defines the class of scala objects its values can have. Note
 * that a variable does *not* define a domain (i.e. subset of values of the class).
 * Instead, modules and models define zero support for particular values. The
 * purpose of this is minimal sharing of resources (such as domain objects)
 * between modules, even if they concern the same variables.
 *
 * @author sriedel
 */
trait Variable[+V] {
  /**
   * This returns the value of a variable with respect to a specified state.
   * Since the state is an implicit parameter, this method allows
   * clients to use variables as if they have a single value, if needed.
   */
  def value(implicit state: State): V = state(this)
}

/**
 * A variable associated with an identifier. This identifier determines the identity of the variable.
 * That is, two Variable objects with same ID will be assigned to the same values by states.
 */
class Var[+V, ID](val identifier: ID) extends Variable[V]

/**
 * A variable that is identified by some predicate name and an argument to this predicate.
 */
case class Atom[A, +V](name: Symbol, arg: A) extends Variable[V] {
}
