package com.github.riedelcastro.theppl

import java.util.UUID
import java.io.{InputStream, OutputStream}

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
  def predict: State = argmax(Message.emtpy)
}


/**
 * A module creates models based on a context and observation. Think of it as a parametrized
 * scoring function s_i(y_i;x_i) where i is the context, and x the observation.
 * @author sriedel
 */
trait Module {
  thisModule =>

  /**
   * The context is an object that (a) parametrizes the scoring function, and (b)
   * determines which hidden (c) determines which hidden and observed
   * variables the scoring function is defined over. For example, for
   * a PoS tagger the context could be a sentence object, and this would define
   * PoS tag variables as hidden and word form variables as observed variables.
   * A crucial difference to observations is that contexts can be arbitrary
   * application objects and don't have to be assignments to variables.
   */
  type Context
  type Hidden <: Variable[Any]
  type Observed <: Variable[Any]
  type ModelType <: Model

  /**
   * A model defines a potential/scoring function over a set
   * of variables.
   */
  trait Model extends com.github.riedelcastro.theppl.Model {
    type Hidden = thisModule.Hidden

    def context: Context

    def observed: Iterable[Observed]

    def observation: State
  }

  /**
   * This class can be instantiated to create proxy objects which can
   * have other traits mixed-in. Eg., you can write "new module.Wrap with Learner".
   */
  class Wrap extends ModuleProxy {
    type Self = Module
    val self = thisModule
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

  /**
   * Returns the name of this module.
   */
  override def toString = name

  /**
   * Serialize this module.
   */
  def save(out: OutputStream)

  /**
   * Deserialize this module.
   */
  def load(in: InputStream)

}

/**
 * A model that scores a single variable in isolation.
 */
trait LocalModule extends Module {
  type Label
  type Hidden <: Variable[Label]
  type ModelType <: LocalModel

  trait LocalModel extends Model {
    def variable: Hidden

    def hidden = Seq(variable)

    def domain: Seq[Label]
  }

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




