package com.github.riedelcastro.theppl

import infer.{SumProductBPRecipe, ExpectatorRecipe}


/**
 * The sum of several argument potentials.
 * @author sriedel
 */
trait SumPotential extends Potential {

  def args: Iterable[Potential]
  def hidden = args.flatMap(_.hidden).toSet.toIndexedSeq
  def score(state: State) = args.map(_.score(state)).sum
}

trait FeatureSumPotential extends SumPotential with FeaturePotential {
  def featureArgs: Iterable[FeaturePotential]
  def otherArgs: Iterable[Potential]
  def args = otherArgs ++ featureArgs
  def features(state: State) = {
    val result = new ParameterVector()
    for (arg <- featureArgs) result.add(arg.features(state), 1.0)
    result
  }
  override def defaultExpectator(cookbook: ExpectatorRecipe[Potential]) =
    new SumProductBPRecipe(10).expectator(this,cookbook)
}

