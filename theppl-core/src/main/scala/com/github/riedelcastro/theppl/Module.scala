package com.github.riedelcastro.theppl

import java.util.UUID

/**
 * A module creates models based on a context.
 * @author sriedel
 */
trait Module {
  self =>

  type Context
  type Hidden <: Variable[_]
  type Observed <: Variable[_]
  type ModelType <: Model

  /**
   * A model defines a potential/scoring function over a set
   * of variables.
   */
  trait Model {
    def context: Context
    def variables: Iterable[Hidden]
    def observed: Iterable[Observed]
    def score(state: State): Double
    def argmax(penalties: Message): State
  }

  def model(context: Context, observation: State): ModelType

  type Pipeable = Module {
    type Context = self.Context
    type Observed = self.Hidden
  }
  type LinearPipeable = LinearModule {
    type Context = self.Context
    type Observed = self.Hidden
  }

  def |(that: Pipeable) = new PipedSimple[Context, Hidden](this, that)
  def |(that: LinearPipeable) = new PipedLinear[Context, Hidden](this, that)
  //def |(that: Pipeable) = pipe(that)

  val name: String = "Module(%s)".format(UUID.randomUUID.toString)

  override def toString = name
}

trait Variable[V] {
  //def domain: Iterable[V]
}

case class Var[V, C](context: C) extends Variable[V]
//abstract class Var[V](val domain: Iterable[V]) extends Variable[V]

class Atom[A, V](val name: Symbol, val arg: A, val domain: Iterable[V]) extends Variable[V] {
  override def equals(that: Any) = that match {
    case a: Atom[_, _] => name == a.name && arg == a.arg
    case _ => false
  }
  override val hashCode = 41 * (41 + name.hashCode) + arg.hashCode
}


trait LinearModule extends Module {

  type ModelType <: LinearModel
  val weights: GlobalParameterVector = new GlobalParameterVector
  trait LinearModel extends Model {
    def features(state: State): GlobalParameterVector
    def featureDelta(gold: State, guess: State) = {
      val result = features(gold)
      result.add(features(guess), -1.0)
      result
    }
    def score(state: State) = features(state) dot weights
  }

}

