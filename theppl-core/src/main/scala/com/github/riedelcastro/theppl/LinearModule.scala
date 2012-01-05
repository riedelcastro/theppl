package com.github.riedelcastro.theppl

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