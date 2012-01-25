package com.github.riedelcastro.theppl

import java.io.{OutputStream, InputStream}
import collection.mutable.HashMap
import math._

/**
 * A LinearModule calculates its score by a dot product of
 * weight vector and feature vector (of the state to score).
 *
 * @author sriedel
 */
trait LinearModule[-Context] extends Module[Context] {
  thisModule =>

  type ModelType <: LinearModule[Context]#LinearModel

  def weights: HierarchicalParameterVector

  trait LinearModel extends com.github.riedelcastro.theppl.LinearModel  {
    def weights = thisModule.weights
  }

}

/**
 * A linear model calculates the score of a state by
 * taking the dot product of a weight vector and a feature representation of the state.
 */
trait LinearModel extends Model {
  def weights: HierarchicalParameterVector
  def features(state: State): HierarchicalParameterVector
  def featureDelta(gold: State, guess: State) = {
    val result = features(gold)
    result.add(features(guess), -1.0)
    result
  }
  def score(state: State) = features(state) dot weights
  def expectations(penalties: Messages):Expectations
}

/**
 * The result of marginalizing a linear model also contains expectations of the features/sufficient statistics.
 */
trait Expectations extends MarginalizeResult {
  def featureExpectations:HierarchicalParameterVector
}

trait BruteForceExpectationCalculator extends FiniteSupportModel with LinearModel {
  def expectations(penalties: Messages) = {

    val masses = new HashMap[(Variable[Any],Any),Double] {
      override def default(key: (Variable[Any], Any)) = 0.0
    }
    val featExp = new HierarchicalParameterVector()
    var total = 0.0
    for (state <- allStates) {
      val mass = math.exp(penalizedScore(penalties,state))
      for (v <- hidden) masses(v -> state(v)) = masses(v -> state(v)) + mass
      total += mass
      featExp.add(features(state), mass)
    }
    featExp.scale( 1.0 / total)
    new Expectations {
      lazy val logMarginals = Messages.fromMap(masses.map(x => x._1 -> (log(x._2)- log(total))))
      lazy val logZ = math.log(total)
      lazy val featureExpectations = featExp
    }

  }
}



/**
 * A LinearModule that has no child modules.
 */
trait LinearLeafModule[Context] extends LinearModule[Context] with SerializableModule[Context] {
  def load(in: InputStream) {
    weights(Seq.empty) = new ParameterVector()
    weights.params(Seq.empty).load(in)
  }
  def save(out: OutputStream) {
    weights.params(Seq.empty).save(out)
  }
}
