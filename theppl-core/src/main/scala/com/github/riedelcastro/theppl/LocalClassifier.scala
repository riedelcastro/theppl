package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait LocalClassifier extends LinearModule {
  type Label
  type Factor = LocalFactor
  type Variable = Var[Context,Label]

  def domain:Iterable[Label]

  def contextFeatures(context:Context) : ParameterVector
  def labelFeatures(label:Label) : ParameterVector

  trait LocalFactor extends LinearFactor {
    val variables = Seq(Var(context,domain))
  }

  def factor(c: Context) = new LocalFactor {
    val context = c
    def argmax(penalties: Message) = null
    def features = null
  }
}