package com.github.riedelcastro.theppl

import util.MathUtil

/**
 * @author sriedel
 */
trait LocalClassifier extends LinearModule {
  module =>
  type Label
  type ModelType = LocalModel
  type Observed = Variable[ParameterVector]
  type Hidden <: Variable[Label]

  def labelFeatures(label: Label): ParameterVector

  trait LocalModel extends LinearModel {
    def labelVariable: Hidden
    val domain: Iterable[Label]
    def variables = Seq(labelVariable)
    def featureVariable: Observed
    def observed = Seq(featureVariable)
    def classify: Label = argmax(null).get(labelVariable).get
    def argmax(penalties: Message): State = {
      val states = domain.map(new SingletonState(labelVariable, _))
      val (state, s) = MathUtil.argmax(states, (s: State) => score(s))
      state
    }
    def features(state: State) = {
      val feats = state.get(featureVariable).get conjoin labelFeatures(state.get(labelVariable).get)
      new GlobalParameterVector(module, feats)
    }
  }


  def classify(context: Context, observation: State): Label = model(context, observation).classify

}

class LocalVariable[L,C](val module:LocalClassifier { type Label = L; type Context = C}, context:C)
  extends Variable[L]

class Classifier[C, V <: Variable[L], L](dom: C => Iterable[L], varCreator: C => V,
                                         featCreator: C => Variable[ParameterVector],
                                         labelFeats: L => ParameterVector = (l: L) => new ParameterVector(Seq(Feat(l))))
  extends LocalClassifier {

  type Label = L
  type Hidden = V
  type Context = C

  def model(c: Context, observed: State): ModelType = new LocalModel {
    def context = c
    val domain = dom(c)
    val featureVariable = featCreator(c)
    val labelVariable = varCreator(c)
  }

  def labelFeatures(label: Label) = labelFeats(label)


}

class FeatureExtractor[C](val creator: C => Variable[ParameterVector],
                          val extractor: C => ParameterVector) extends Module {
  type Observed = Variable[Nothing]
  type Hidden = Variable[ParameterVector]
  type Context = C
  type ModelType = ExtractorModel

  class ExtractorModel(val context: Context, val variable: Variable[ParameterVector]) extends Model {
    lazy val cachedFeats = extractor(context)
    lazy val cachedArgmax = new SingletonState(variable, cachedFeats)
    def argmax(penalties: Message) = cachedArgmax
    def score(state: State) = if (state.get(variable).get == cachedFeats) 0.0 else Double.NegativeInfinity
    def observed = Seq.empty
    def variables = Seq(variable)
  }

  def model(c: Context, observed: State) = new ExtractorModel(c, creator(c)) {}

}