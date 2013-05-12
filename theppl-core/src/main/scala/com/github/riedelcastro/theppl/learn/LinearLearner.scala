package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl.term.{TermImplicits, Vec, DenseVec, Loglinear}
import com.github.riedelcastro.theppl.State
import com.github.riedelcastro.theppl.infer.Argmaxer

/**
 * @author Sebastian Riedel
 */
object LinearLearner {

  import TermImplicits._

  def learn(model: Loglinear)(instances: Seq[State]): Vec = {
    val weights = new DenseVec(1)
    for (epochs <- 0 until 2) {
      for (instance <- instances) {
        //create one "observed" model per instance. This model, when compiled, could
        //remember the feature vectors for each state.
        val conditioned = (model | instance) | model.weights -> weights
        val argmaxer = Argmaxer(conditioned)
        val guess = argmaxer.argmax().state
        val gold = instance.target
        val guessFeats = (model.features | instance).eval(guess).get
        val goldFeats = (model.features | instance).eval(gold).get
        weights.add(goldFeats, 1.0)
        weights.add(guessFeats, -1.0)
      }
    }
    weights
  }
}
