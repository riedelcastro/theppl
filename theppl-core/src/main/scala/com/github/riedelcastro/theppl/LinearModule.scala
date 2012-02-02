package com.github.riedelcastro.theppl

import java.io.{OutputStream, InputStream}

/**
 * A LinearModule calculates its score by a dot product of
 * weight vector and feature vector (of the state to score).
 *
 * @author sriedel
 */
trait LinearModule[-Context] extends Module[Context] {
  thisModule =>

  type ModelType <: LinearModule[Context]#LinearModel

  def weights: ParameterVector

  trait LinearModel extends com.github.riedelcastro.theppl.LinearModel {
    def weights = thisModule.weights
  }

}

/**
 * A FeatureModel can calculate a feature representation of a state. This
 * representation is often  used to calculate the score of the model,
 * but this interface does not require any connection between score
 * and features.
 */
trait FeatureModel extends Model {
  thisModel =>
  def features(state: State): ParameterVector
  def featureDelta(gold: State, guess: State) = {
    val result = features(gold)
    result.add(features(guess), -1.0)
    result
  }

  def defaultExpectator(cookbook: ExpectatorRecipe[Model]): Expectator = new BFExpectator {
    val model = thisModel
  }

}

/**
 * A linear model calculates the score of a state by
 * taking the dot product of a weight vector and a feature representation of the state.
 */
trait LinearModel extends FeatureModel {
  def weights: ParameterVector
  def score(state: State) = features(state) dot weights
}


/**
 * A LinearModule that has no child modules.
 */
trait LinearLeafModule[Context] extends LinearModule[Context] with SerializableModule[Context] {
  def load(in: InputStream) {
    weights.load(in)
  }
  def save(out: OutputStream) {
    weights.save(out)
  }
}
