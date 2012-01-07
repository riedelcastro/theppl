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
  trait LinearModel extends Model {
    def features(state: State): GlobalParameterVector
    def featureDelta(gold: State, guess: State) = {
      val result = features(gold)
      result.add(features(guess), -1.0)
      result
    }
    def score(state: State) = features(state) dot weights
  }

  class decorated extends super.decorated with LinearModule {
    def weights = thisModule.weights
  }
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
