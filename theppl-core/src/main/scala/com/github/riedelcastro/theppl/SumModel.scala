package com.github.riedelcastro.theppl

/**
 * The sum of several argument models.
 * @author sriedel
 */
trait SumModel extends Model {

  def args: Iterable[Model]
  def hidden = args.flatMap(_.hidden).toSet.toIndexedSeq
  def score(state: State) = args.map(_.score(state)).sum
}

trait LinearSumModel extends SumModel with LinearModel {

  def linearArgs:Iterable[LinearModel]
  def opaqueArgs:Iterable[Model]
  def args = linearArgs ++ opaqueArgs

  override def score(state: State) = {
    super[SumModel].score(state)
  }
  def features(state: State) = {
    val result = new ParameterVector()
    for (arg <- linearArgs) result.add(arg.features(state),1.0)
    result
  }
}
