package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */

trait Variable[+V] {
  //def domain: Iterable[V]
}

case class Var[+V, C](context: C) extends Variable[V]
//abstract class Var[V](val domain: Iterable[V]) extends Variable[V]

case class Atom[A, +V](name: Symbol, arg: A) extends Variable[V] {
}
