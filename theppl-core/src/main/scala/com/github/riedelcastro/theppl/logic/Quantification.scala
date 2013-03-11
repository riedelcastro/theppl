package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{ParameterVector, State, Variable}

/**
 * @author Sebastian Riedel
 */
trait Quantification[T, R, This <: Quantification[T, R, This]] extends Term[R] {
  this: This =>

  def arguments: Seq[Variable[Any]]
  def term: Term[T]
  def create(vars: Seq[Variable[Any]], newTerm: Term[T]): This
  def aggregator: Term[Seq[T] => R]
  def eval(state: State) = {
    val parts = State.allStates(arguments).map(s => term.eval(s + state))
    val aggregator = this.aggregator.eval(state)
    if (aggregator.isEmpty || parts.exists(_.isEmpty)) None else Some(aggregator.get(parts.map(_.get)))
  }
  def variables = (term.variables.toSet -- arguments).toSeq
  override def substitute(substitution: Substitution) = {
    //todo: need to consider that substitutions have free variables
    val fresh = (substitution.variables -- arguments)
    val subset = substitution.subset(fresh)
    create(arguments, term.substitute(subset))
  }
  override def ground = {
    val parts = Substitution.allGroundings(arguments).map(term.substitute(_).ground)
    FunApp1(aggregator, SeqTerm(parts))
  }
  def default = aggregator.default(Seq(term.default))
}

case class Forall(arguments: Seq[Variable[Any]], term: Term[Boolean])
  extends Quantification[Boolean, Boolean, Forall] {
  def create(vars: Seq[Variable[Any]], newTerm: Term[Boolean]) = Forall(vars, newTerm)
  def aggregator = AndN
}

case class Exists(arguments: Seq[Variable[Any]], term: Term[Boolean])
  extends Quantification[Boolean, Boolean, Exists] {
  def create(vars: Seq[Variable[Any]], newTerm: Term[Boolean]) = Exists(vars, newTerm)
  def aggregator = OrN
}

case class QuantifiedSum(arguments: Seq[Variable[Any]], term: Term[Double])
  extends Quantification[Double, Double, QuantifiedSum] {
  def create(vars: Seq[Variable[Any]], newTerm: Term[Double]) = QuantifiedSum(vars, newTerm)
  def aggregator = DoubleAddN
}

case class QuantifiedVectorSum(arguments: Seq[Variable[Any]], term:Term[ParameterVector])
  extends Quantification[ParameterVector,ParameterVector,QuantifiedVectorSum] {
  def create(vars: Seq[Variable[Any]], newTerm: Term[ParameterVector]) = QuantifiedVectorSum(vars,newTerm)
  def aggregator = ParameterVectorAddN
}