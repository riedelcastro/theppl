package com.github.riedelcastro.theppl

import java.io.{OutputStream, InputStream}

/**
 * @author sriedel
 */
trait LinearModule extends Module { thisModule =>

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

