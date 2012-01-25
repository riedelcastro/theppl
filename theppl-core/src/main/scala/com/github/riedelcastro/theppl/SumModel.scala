package com.github.riedelcastro.theppl

/**
 * The sum of several argument models.
 * @author sriedel
 */
trait SumModel extends Model {

  type ArgType <: Model

  def args: Iterable[ArgType]
  def hidden = args.flatMap(_.hidden).toSet.toIndexedSeq
  def score(state: State) = args.map(_.score(state)).sum
}

trait LinearSumModel extends SumModel with LinearModel {

  type ArgType <: LinearModel

  override def score(state: State) = {
    super[SumModel].score(state)
  }

  def features(state: State) = {
    val result = new ParameterVector()
    for (arg <- args) result.add(arg.features(state),1.0)
    result
  }
}
