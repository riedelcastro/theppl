package com.github.riedelcastro.theppl

/**
 * The sum of several argument models.
 * @author sriedel
 */
trait SumModel extends Model {
  type ArgType <: Model

  def args: Iterable[ArgType]

  def score(state: State) = args.map(_.score(state)).sum
}

