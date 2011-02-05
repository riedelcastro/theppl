package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait OnlineLearner extends LinearModule {

  def epochs: Int = 2

  def train[C](instances: Seq[TrainingInstance[Context]]) {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val gold = instance.state
        val model = this.model(instance.context, null)
        val guess = model.argmax(null)
        val delta = model.featureDelta(gold,guess)
        weights.add(delta, 1.0)
      }
    }
  }

}

class TrainingInstance[C](val context: C, val state: State) {

}

class OnlineLearner2(val module:LinearModule) {

  var epochs: Int = 2

  def train[C](instances: Seq[TrainingInstance[module.Context]]) {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val gold = instance.state
        val model = module.model(instance.context, null)
        val guess = model.argmax(null)
        val delta = model.featureDelta(gold,guess)
        module.weights.add(delta, 1.0)
      }
    }
  }

}