package com.github.riedelcastro.theppl

/**
 * A module creates factors based on a context.
 * @author sriedel
 */
trait Module {

  type Context
  type Variable <: Var[_,_]
  type Factor <: ModuleFactor

  /**
   * A factor defines a potential/scoring function over a set
   * of variables.
   */
  trait ModuleFactor {
    def context:Context
    def variables:Iterable[Variable]
    def score(state:State):Double
    def argmax(penalties:Message):State
  }

  def factor(context:Context):Factor
}

/**
 * A Variable with an id and domain.
 */
case class Var[I,V](id:I, domain:Iterable[V]){
}

trait Message {
  def msg[V](variable:Var[_,V], value:V):Double
}

trait State extends Message {
  def get[V](variable:Var[_,V]) : Option[V] = {
    variable.domain.find(value => msg(variable,value) == 1.0)
  }
}

trait Marginals extends Message {
  def marginals[V](variable:Var[_,V], value:V) = msg(variable,value)
}


trait LinearModule extends Module {
  type Factor <: LinearFactor
  def weights:GlobalParameterVector = new GlobalParameterVector
  trait LinearFactor extends ModuleFactor {
    def features(state:State):GlobalParameterVector
    def score(state: State) = features(state) dot weights
  }
}

