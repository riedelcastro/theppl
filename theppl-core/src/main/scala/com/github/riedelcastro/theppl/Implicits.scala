package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait Implicits {
  implicit def pairsToParamVector(pairs:Iterable[(Any,Any)]):ParameterVector = {
    new ParameterVector(pairs.map({case (key,value) => Feat(key,value)}))
  }
  implicit def featsToParamVector(feats:Iterable[Feat]):ParameterVector = {
    new ParameterVector(feats)
  }
}

object Implicits extends Implicits