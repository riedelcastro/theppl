package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{Variable, State}
import collection.SeqProxy


/**
 * A term evaluates to a value given a possible world/state.
 * @author sriedel
 */
trait Term[V] {
  def eval(state: State): V
}

/**
 * A term that evaluates to the value associated with the variable.
 */
case class VarTerm[V](variable: Variable[V]) extends Term[V] {
  def eval(state: State) = state(variable)
}

case class ConstantTerm[V](value:V) extends Term[V] {
  def eval(state: State) = value
}

/**
 * A collection of terms.
 */
trait TermFamily[V, T <: Term[V]] extends Iterable[T] {

}

case class Domain[V](values:Seq[V]) extends TermFamily[V, ConstantTerm[V]] with SeqProxy[ConstantTerm[V]] {
  
  val self = values.map(ConstantTerm(_))
}