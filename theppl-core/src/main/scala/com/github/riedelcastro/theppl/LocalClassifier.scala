package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait LocalClassifier extends LinearModule {
  module =>
  type Label
  type Factor = LocalFactor
  type Var <: Variable[Label]
  type Context <: Var

  def contextFeatures(context: Context): ParameterVector
  def labelFeatures(label: Label): ParameterVector

  trait LocalFactor extends LinearFactor {
    val variable = context
    val variables = Seq(variable)
    def classify: Label = argmax(null).get(variable).get
    def argmax(penalties: Message): State = null
    def features(state: State) = {
      val feats = contextFeatures(context) conjoin labelFeatures(state.get(variable).get)
      new GlobalParameterVector(module, feats)
    }
  }

  def factor(c: Context): Factor = new LocalFactor {
    val context = c
  }

  def classify(context: Context): Label = factor(context).classify

}

class Classifier[V <: Variable[L], L](contextFeats: V => ParameterVector,
                                      labelFeats: L => ParameterVector = (l: L) => new ParameterVector(Seq(l))) extends LocalClassifier {
  type Label = L
  type Var = V
  type Context = V

  def contextFeatures(context: Context) = contextFeats(context)
  def labelFeatures(label: Label) = labelFeats(label)
}