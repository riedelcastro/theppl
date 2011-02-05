package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait LinearModule extends Module {

  type ModelType <: LinearModel
  val weights: GlobalParameterVector = new GlobalParameterVector
  trait LinearModel extends Model {
    def features(state: State): GlobalParameterVector
    def featureDelta(gold: State, guess: State) = {
      val result = features(gold)
      result.add(features(guess), -1.0)
      result
    }
    def score(state: State) = features(state) dot weights
  }

}
