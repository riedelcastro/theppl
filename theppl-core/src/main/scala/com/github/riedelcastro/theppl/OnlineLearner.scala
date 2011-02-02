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
        val factor = this.factor(instance.context)
        val guess = factor.argmax(null)
        val delta = factor.featureDelta(gold,guess)
        weights.add(delta, 1.0)
      }
    }
  }

}

class TrainingInstance[C](val context: C, val state: State) {

}
