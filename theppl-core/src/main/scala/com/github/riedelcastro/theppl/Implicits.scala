package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait Implicits {
  implicit def pairToSingletonState[V](pair:(Variable[V],V)) = State.singleton(pair._1,pair._2)
  implicit def pairsToParamVector(pairs:Iterable[(Any,Any)]):ParameterVector = {
    new ParameterVector(pairs.map({case (key,value) => Feat(key,value)}))
  }
  implicit def featsToParamVector(feats:Iterable[Feat]):ParameterVector = {
    new ParameterVector(feats)
  }
  implicit def featToParamVector(feat:Feat):ParameterVector = {
    new ParameterVector(Iterable(feat))
  }

}

object Implicits extends Implicits