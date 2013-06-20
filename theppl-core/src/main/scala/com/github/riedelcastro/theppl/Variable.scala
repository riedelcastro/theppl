package com.github.riedelcastro.theppl

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.util.{CollectionUtil, SetUtil, Util}


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

case class LabelVar[Id,L](id:Id,domain:Seq[L]) extends Variable[L] {
  override def toString = "L(" + id.toString + ")"
}

case class StringVar[Id](id:Id) extends Variable[String] {
  def domain = Util.infinity
}

case class VectorVar[Id](id:Id) extends Variable[ParameterVector] {
  def domain = Util.infinity
  override def default = new ParameterVector
}

case class VecVar[Id](id:Id) extends Variable[Vec] {
  def domain = Util.infinity
  override def default = Vec.zero
}

object Variables {


  object All extends Set[Variable[Any]] {
    def contains(elem: Variable[Any]) = true
    def +(elem: Variable[Any]) = this
    def -(elem: Variable[Any]) = SetUtil.SetMinus(this,Set(elem))
    def iterator = Util.infinity
  }

  case class AllAtoms(predicates:Set[Pred[_,_]]) extends Set[Variable[Any]] {
    def contains(elem: Variable[Any]) = elem match {
      case GroundAtom(pred,_) => predicates(pred)
      case _ => false
    }
    def +(elem: Variable[Any]) = SetUtil.Union(Set(this,Set(elem)))
    def -(elem: Variable[Any]) = SetUtil.SetMinus(this,Set(elem))
    def iterator = predicates.iterator.flatMap(_.variables.toIterator)
  }

  case class PartialAtoms(pred:Pred[_,_],args:Seq[Term[Any]], condition:State = State.empty) extends Set[Variable[Any]] {
    lazy val evaluated = args.map(_.eval(condition))
    lazy val domains = evaluated.zip(pred.domains).map({case (Some(value),_) => Set(value); case (_,dom) => dom.values.toSet})
    def contains(elem: Variable[Any]) = elem match {
      case GroundAtom(p,args) if (p == pred) => args.zip(domains).forall({case (arg,dom) => dom(arg)})
      case _ => false
    }
    def +(elem: Variable[Any]) = SetUtil.Union(Set(this,Set(elem)))
    def -(elem: Variable[Any]) = SetUtil.SetMinus(this,Set(elem))
    def iterator = CollectionUtil.allTuples(domains).map(pred.genericMapping).toIterator
    def conditioned(state:State) = copy(condition = condition + state)
  }




}
