package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait ModuleProxy extends Module {
  module =>
  val self: Module
  type ModelType = ModelProxy
  type Observed = self.Observed
  type Hidden = self.Hidden
  type Context = self.Context
  trait ModelProxy extends com.github.riedelcastro.theppl.ModelProxy with Model {
    override type Hidden = module.Hidden
    val self: module.self.ModelType
    def observation = self.observation
    def observed = self.observed
    def context = self.context
  }
  def model(context: Context, observation: State): ModelType =
    new ModelProxy {
      val self = module.self.model(context, observation)
    }
}

trait ModelProxy extends Model {
  val self: Model
  def argmax(penalties: Message) = self.argmax(penalties)
  def score(state: State) = self.score(state)
  def hidden = self.hidden
  type Hidden = self.Hidden
}