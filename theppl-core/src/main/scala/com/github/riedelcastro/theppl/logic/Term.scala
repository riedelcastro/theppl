package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{Variable, State}
import com.github.riedelcastro.theppl.util.EmptyBuilt


/**
 * A term evaluates to a value given a possible world/state.
 * @author sriedel
 */
trait Term[+V] {
  def eval(state: State): V
  def variables: Iterable[Variable[Any]]
}

/**
 * A term that evaluates to the value associated with the variable.
 */
case class VarTerm[+V](variable: Variable[V]) extends Term[V] {
  def eval(state: State) = state(variable)
  def variables = Seq(variable)
}

case class Constant[+V](value: V) extends Term[V] {
  def eval(state: State) = value
  def variables = Seq.empty
}

case class FunApp[A, R](f: Term[A => R], arg: Term[A]) extends Term[R] {
  def variables = (f.variables ++ arg.variables).toSet
  def eval(state: State) = f.eval(state)(arg.eval(state))
}

class UniqueVar[T](name: String) extends Variable[T] with Term[T] {
  def eval(state: State) = state(this)
  def variables = Seq(this)
}

//case class Dom[T](values: Seq[T]) extends EmptyBuilt[UniqueVar[T]] {
//  private var count = 0
//  def newName() = {count += 1; "x" + count}
//  def argument = new UniqueVar[T](newName())
//}

case class TupleTerm2[T1,T2](arg1:Term[T1], arg2:Term[T2]) extends Term[(T1, T2)] {
  def eval(state: State) = (arg1.eval(state),arg2.eval(state))
  def variables = (arg1.variables ++ arg2.variables).toSet
}

object LogicPlayground {

  def $[T](t: T):Constant[T] = Constant(t)
//  def $[A1,A2,R](f: (A1,A2)=>R):Constant[(A1,A2)=>R] = new Constant(f) with Function2[Term[A1],Term[A2],FunApp[(A1, A2),R]]{
//    def apply(arg1:Term[A1],arg2:Term[A2]) = FunApp(this,TupleTerm2(arg1,arg2))
//  }

  
  implicit def toTT2[T1,T2](pair:(Term[T1],Term[T2])) = TupleTerm2(pair._1,pair._2)

  def main(args: Array[String]) {
    val values = Range(0, 10)
    val add = (x: (Int, Int)) => x._1 + x._2
//    val formula = for (x <- Dom(values); y <- Dom(values)) yield FunApp($(add), (x, y))
  }
}
