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

trait FeatureSumTemplate[Context] extends LinearTemplate[Context] { template =>

  type FeatureArgumentContext
  type OtherArgumentContext
  type PotentialType <: TemplatedFeatureSumPotential

  def featureArgs(context:Context):Seq[FeatureArgumentContext]
  def otherArgs(context:Context):Seq[OtherArgumentContext]

  def featureTemplate:LinearTemplate[FeatureArgumentContext]
  def otherTemplate:Template[OtherArgumentContext]

  trait TemplatedFeatureSumPotential extends FeatureSumPotential with LinearPotential {
    override def score(state: State) = super.score(state)
    def context:Context
    lazy val featureContexts = template.featureArgs(context)
    lazy val featureArgs = featureContexts.map(ac => featureTemplate.potential(ac))
    lazy val otherContexts = template.otherArgs(context)
    lazy val otherArgs = otherContexts.map(ac => otherTemplate.potential(ac))

  }

  def weights = featureTemplate.weights

}

