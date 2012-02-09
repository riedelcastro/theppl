package com.github.riedelcastro.theppl

import infer.{SumProductBPRecipe, ExpectatorRecipe}


/**
 * The sum of several argument models.
 * @author sriedel
 */
trait SumModel extends Model {

  def args: Iterable[Model]
  def hidden = args.flatMap(_.hidden).toSet.toIndexedSeq
  def score(state: State) = args.map(_.score(state)).sum
}



trait FeatureSumModel extends SumModel with FeatureModel {
  def featureArgs: Iterable[FeatureModel]
  def otherArgs: Iterable[Model]
  def args = otherArgs ++ featureArgs
  def features(state: State) = {
    val result = new ParameterVector()
    for (arg <- featureArgs) result.add(arg.features(state), 1.0)
    result
  }
  override def defaultExpectator(cookbook: ExpectatorRecipe[Model]) = SumProductBPRecipe.expectator(this,cookbook)
}

