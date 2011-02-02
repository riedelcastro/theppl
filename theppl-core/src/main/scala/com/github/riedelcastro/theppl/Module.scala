package com.github.riedelcastro.theppl

/**
 * A module creates factors based on a context.
 * @author sriedel
 */
trait Module {

  type Context
  type Var <: Variable[_]
  type Factor <: ModuleFactor

  /**
   * A factor defines a potential/scoring function over a set
   * of variables.
   */
  trait ModuleFactor {
    def context: Context
    def variables: Iterable[Var]
    def score(state: State): Double
    def argmax(penalties: Message): State
  }

  def factor(context: Context): Factor
}

trait Variable[V] {
  def domain:Iterable[V]
}

abstract class Var[V](val domain:Iterable[V]) extends Variable[V]




trait LinearModule extends Module {
  type Factor <: LinearFactor
  def weights: GlobalParameterVector = new GlobalParameterVector
  trait LinearFactor extends ModuleFactor {
    def features(state: State): GlobalParameterVector
    def featureDelta(gold:State, guess:State) = {
      val result = features(gold)
      result.add(features(guess),-1.0)
      result
    }
    def score(state: State) = features(state) dot weights
  }
}

