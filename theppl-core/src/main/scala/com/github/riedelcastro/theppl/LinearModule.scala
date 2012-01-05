package com.github.riedelcastro.theppl

import java.io.{OutputStream, InputStream}

/**
 * @author sriedel
 */
trait LinearModule extends Module { thisModule =>

  type ModelType <: LinearModel
  def weights: GlobalParameterVector
  trait LinearModel extends Model {
    def features(state: State): GlobalParameterVector
    def featureDelta(gold: State, guess: State) = {
      val result = features(gold)
      result.add(features(guess), -1.0)
      result
    }
    def score(state: State) = features(state) dot weights
  }
  
  class Wrap extends LinearModuleProxy {
    val self = thisModule
  }
  
  class decorated extends LinearModule {
    type Context = thisModule.Context
    type Hidden = thisModule.Hidden
    type Observed = thisModule.Observed
    type ModelType = LinearModel
    def weights = thisModule.weights
    def model(context: Context, observation: State) =
      thisModule.model(context,observation).asInstanceOf[LinearModel]

    def save(out: OutputStream) {
      thisModule.save(out)
    }

    def load(in: InputStream) {
      thisModule.load(in)
    }
  }

}

trait AbstractLinearModuleProxy extends LinearModule with AbstractModuleProxy {
  module =>

  type Self <: LinearModule
  type ModelType <: LinearModelProxy
  def weights = self.weights
  trait LinearModelProxy extends LinearModel with ModelProxy {
    override val self: module.self.ModelType
    def features(state: State) = self.features(state)
    override def featureDelta(gold: State, guess: State) = self.featureDelta(gold, guess)
    override def score(state: State) = super.score(state)
  }

}

trait LinearModuleProxy extends AbstractLinearModuleProxy {
  module =>
  type Self = LinearModule
  type ModelType = LinearModelProxy
  override def model(context: Context, observation: State) = new LinearModelProxy {
    val self = module.self.model(context, observation)
  }

}