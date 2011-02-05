package com.github.riedelcastro.theppl

import java.util.UUID

/**
 * A Model is a scoring function s(Y=y) over a set of variables Y.
 */
trait Model {
  /**
   * The type of hidden variables
   */
  type Hidden <: Variable[_]
  /**
   * The set of hidden variables
   */
  def hidden: Iterable[Hidden]
  /**
   * Returns a score s(y) for each assignment to the hidden variables of this model.
   */
  def score(state: State): Double

  /**
   * Returns the assignment to hidden variables of this model that maximizes the score,
   * with added penalties on the variables.
   */
  def argmax(penalties: Message): State

  /**
   * Convenience method for when no incoming message is needed.
   */
  def predict:State = argmax(Message.emtpy)
}


/**
 * A module creates models based on a context and observation. Think of it as a parametrized
 * scoring function s_i(y_i;x_i) where i is the context, and x the observation.
 * @author sriedel
 */
trait Module {
  self =>

  type Context
  type Hidden <: Variable[Any]
  type Observed <: Variable[Any]
  type ModelType <: Model

  /**
   * A model defines a potential/scoring function over a set
   * of variables.
   */
  trait Model extends com.github.riedelcastro.theppl.Model {
    type Hidden = self.Hidden
    def context: Context
    def observed: Iterable[Observed]
    def observation: State
  }

  /**
   * For a given context i, and observation x, this method creates
   * a model s_i(y;x).
   */
  def model(context: Context, observation: State): ModelType

  /**
   * The name of this module.
   */
  val name: String = "Module(%s)".format(UUID.randomUUID.toString)

  override def toString = name
}


/**
 * A module that does not take any observation into account.
 */
trait SourceModule extends Module {
  type Observed = Variable[Nothing]
  type ModelType <: SourceModel
  trait SourceModel extends Model {
    def observation = State.empty
    def observed = Seq.empty
  }

}



/**
 * Additional convenience methods to mix into modules.
 */
trait ModuleExtras extends Module {
  self =>

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


}




