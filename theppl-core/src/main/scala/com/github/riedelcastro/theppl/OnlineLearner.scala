package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait OnlineLearner extends LinearModule {

  def epochs: Int = 10

  def train[C](instances: Seq[TrainingInstance[Context]]) {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val gold = instance.state
        val model = this.model(instance.context)
        val guess = model.argmax(null)
        val delta = model.featureDelta(gold,guess)
        weights.add(delta, 1.0)
      }
    }
  }

}

class TrainingInstance[C](val context: C, val state: State) {

}
