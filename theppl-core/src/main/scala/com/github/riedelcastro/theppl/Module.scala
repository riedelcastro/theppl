package com.github.riedelcastro.theppl

import java.util.UUID
import java.io.{InputStream, OutputStream}

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

  type ModelType <: Module#Model

  /**
   * A model defines a potential/scoring function over a set
   * of variables.
   */
  trait Model extends com.github.riedelcastro.theppl.Model { thisModel =>

    def context: Context
    def observed: Iterable[Variable[Any]]
    def observation: State
    
    class decorated extends super.decorated with Model {
      def context = thisModel.context
      def observed = thisModel.observed
      def observation = thisModel.observation
    }
    
  }

  /**
   * This class can be instantiated to create proxy objects which can
   * have other traits mixed-in. Eg., you can write "new module.decorated with Learner".
   */
  class decorated extends Module {
    type Context = thisModule.Context
    type ModelType = thisModule.ModelType

    def model(context: Context, observation: State) = thisModule.model(context, observation)

  }

  /**
   * For a given context i, and observation x, this method creates
   * a model s_i(y;x).
   */
  def model(context: Context, observation: State = State.empty): ModelType

  /**
   * The name of this module.
   */
  val name: String = "Module(%s)".format(UUID.randomUUID.toString)

  /**
   * Returns the name of this module.
   */
  override def toString = name


}

trait ProfilerModule extends Module {

  type ModelType = ProfiledModel

  trait ProfiledModel extends Model {

    abstract override def argmax(penalties: Message) = {
      super.argmax(penalties)
    }
  }

  abstract override def model(context: Context, observation: State) = {
    val m = super.model(context,observation)
    new m.decorated with ProfiledModel
  }
}

/**
 * A module that can be stored to and loaded from output and input streams, respectively.
 */
trait SerializableModule extends Module {
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
//    type Observed = self.Hidden
  }
  type LinearPipeable = LinearModule {
    type Context = self.Context
//    type Observed = self.Hidden
  }

  //  def |(that: Pipeable) = new PipedSimple[Context, Hidden](this, that)
  //
  //  def |(that: LinearPipeable) = new PipedLinear[Context, Hidden](this, that)


}




