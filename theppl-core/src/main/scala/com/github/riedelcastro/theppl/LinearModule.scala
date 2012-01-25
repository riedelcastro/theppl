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

  trait LinearModel extends com.github.riedelcastro.theppl.LinearModel  {
    def weights = thisModule.weights
  }

}

/**
 * A linear model calculates the score of a state by
 * taking the dot product of a weight vector and a feature representation of the state.
 */
trait LinearModel extends Model {
  def weights: ParameterVector
  def features(state: State): ParameterVector
  def score(state: State) = features(state) dot weights
  def featureDelta(gold: State, guess: State) = {
    val result = features(gold)
    result.add(features(guess), -1.0)
    result
  }
}

trait HiddenParameters extends LinearModel {
  
  abstract override def score(state: State) = super.features(state) dot super.weights
  abstract override val weights = new ParameterVector()
  abstract override def features(state: State) = new ParameterVector()
}


/**
 * The result of marginalizing a linear model also contains expectations of the features/sufficient statistics.
 */
trait Expectations extends MarginalizeResult {
  def featureExpectations:ParameterVector
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
