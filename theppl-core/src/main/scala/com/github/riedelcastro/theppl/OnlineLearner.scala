package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait OnlineLearner extends LinearModule { self =>

  var epochs: Int = 2

  def train[C](instances: Seq[Instance[Context]]) {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val gold = instance.gold
        val model = self.model(instance.context, instance.observation)
        val guess = model.argmax(null)
        val delta = model.featureDelta(gold, guess)
        weights.add(delta, 1.0)
      }
    }
  }

}

class Corpus(val module:Module)

class Instance[C](val context: C, val gold:State, val observation: State=State.empty) {

}

