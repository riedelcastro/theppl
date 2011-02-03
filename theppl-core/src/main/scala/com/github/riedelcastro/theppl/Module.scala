package com.github.riedelcastro.theppl

import java.util.UUID

/**
 * A module creates models based on a context.
 * @author sriedel
 */
trait Module {

  type Context
  type Var <: Variable[_]
  type Model <: ModuleModel

  /**
   * A model defines a potential/scoring function over a set
   * of variables.
   */
  trait ModuleModel {
    def context: Context
    def variables: Iterable[Var]
    def score(state: State): Double
    def argmax(penalties: Message): State
  }

  def model(context: Context): Model

  val name:String = "Module(%s)".format(UUID.randomUUID.toString)

  override def toString = name
}

trait Variable[V] {
  def domain:Iterable[V]
}

abstract class Var[V](val domain:Iterable[V]) extends Variable[V]




trait LinearModule extends Module {
  type Model <: LinearModel
  val weights: GlobalParameterVector = new GlobalParameterVector
  trait LinearModel extends ModuleModel {
    def features(state: State): GlobalParameterVector
    def featureDelta(gold:State, guess:State) = {
      val result = features(gold)
      result.add(features(guess),-1.0)
      result
    }
    def score(state: State) = features(state) dot weights
  }
}

