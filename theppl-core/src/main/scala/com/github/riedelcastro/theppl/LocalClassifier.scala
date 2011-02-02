package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait LocalClassifier extends LinearModule {
  module =>
  type Label
  type Factor = LocalFactor
  type Variable = Var[Context, Label]

  def domain: Iterable[Label]

  def contextFeatures(context: Context): ParameterVector
  def labelFeatures(label: Label): ParameterVector

  trait LocalFactor extends LinearFactor {
    val variable = Var(context, domain)
    val variables = Seq(variable)
  }

  def factor(c: Context): Factor = new LocalFactor {
    val context = c
    def argmax(penalties: Message) = null
    def features(state: State) = {
      val feats = contextFeatures(context) conjoin labelFeatures(state.get(variable).get)
      new GlobalParameterVector(module, feats)
    }
  }
}

class Classifier[C, L](dom: Iterable[L],
                       contextFeats: C => ParameterVector,
                       labelFeats: L => ParameterVector = (l:L) => new ParameterVector(Seq(l))) extends LocalClassifier {
  type Label = L
  type Context = C

  def contextFeatures(context: Context) = contextFeats(context)
  def labelFeatures(label: Label) = labelFeats(label)
  def domain = dom
}