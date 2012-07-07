package com.github.riedelcastro.theppl

import infer.{Expectator, ExpectatorRecipe, BFExpectator}
import java.io.{OutputStream, InputStream}

/**
 * A LinearTemplate calculates its score by a dot product of
 * weight vector and feature vector (of the state to score).
 *
 * @author sriedel
 */
trait LinearTemplate[-Context] extends Template[Context] {
  thisTemplate =>

  type PotentialType <: LinearTemplate[Context]#LinearPotential

  def weights: ParameterVector

  trait LinearPotential extends com.github.riedelcastro.theppl.LinearPotential {
    def weights = thisTemplate.weights
  }

}

/**
 * A FeaturePotential can calculate a feature representation of a state. This
 * representation is often  used to calculate the score of the potential,
 * but this interface does not require any connection between score
 * and features.
 */
trait FeaturePotential extends Potential {
  thisPotential =>
  def features(state: State): ParameterVector
  def featureDelta(gold: State, guess: State) = {
    val result = features(gold)
    result.add(features(guess), -1.0)
    result
  }

  def defaultExpectator(cookbook: ExpectatorRecipe[Potential] = Expectator): Expectator = new BFExpectator {
    val potential = thisPotential
  }

  def expectations = defaultExpectator().expectations()

}

/**
 * A linear potential calculates the score of a state by
 * taking the dot product of a weight vector and a feature representation of the state.
 */
trait LinearPotential extends FeaturePotential {
  def weights: ParameterVector
  def score(state: State) = features(state) dot weights
  def targetExpectations = features(truth)
}


/**
 * A LinearTemplate that has no child templates.
 */
trait LinearLeafTemplate[Context] extends LinearTemplate[Context] with SerializableTemplate[Context] {
  def load(in: InputStream) {
    weights.load(in)
  }
  def save(out: OutputStream) {
    weights.save(out)
  }
}
