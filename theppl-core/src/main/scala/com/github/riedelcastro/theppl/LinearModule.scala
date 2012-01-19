package com.github.riedelcastro.theppl

import java.io.{OutputStream, InputStream}

/**
 * A LinearModule calculates its score by a dot product of
 * weight vector and feature vector (of the state to score).
 *
 * @author sriedel
 */
trait LinearModule extends Module {
  thisModule =>

  type ModelType <: LinearModule#LinearModel

  def weights: GlobalParameterVector

  trait LinearModel extends com.github.riedelcastro.theppl.LinearModel with Model {
    def weights = thisModule.weights
  }

  class decorated extends super.decorated with LinearModule {
    def weights = thisModule.weights
  }
}

/**
 * A linear model calculates the score of a state by
 * taking the dot product of a weight vector and a feature representation of the state.
 */
trait LinearModel extends Model {
  def weights: GlobalParameterVector
  def features(state: State): GlobalParameterVector
  def featureDelta(gold: State, guess: State) = {
    val result = features(gold)
    result.add(features(guess), -1.0)
    result
  }
  def score(state: State) = features(state) dot weights

}

/**
 * A LinearModule that has no child modules.
 */
trait LinearLeafModule extends LinearModule with SerializableModule {
  def load(in: InputStream) {
    weights(Seq.empty) = new ParameterVector()
    weights.params(Seq.empty).load(in)
  }
  def save(out: OutputStream) {
    weights.params(Seq.empty).save(out)
  }
}
