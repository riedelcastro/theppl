package com.github.riedelcastro.theppl

import util.MathUtil

/**
 * @author sriedel
 */
trait LocalClassifier extends LinearModule {
  module =>
  type Label
  type ModelType <: LocalModel
  type Hidden <: Variable[Label]

  val weights = new GlobalParameterVector

  trait LocalModel extends LinearModel {
    def labelFeatures(label: Label): ParameterVector
    def contextFeatures: ParameterVector
    def labelVariable: Hidden
    val domain: Iterable[Label]
    def hidden = Seq(labelVariable)
    def observation: State
    def classify: Label = argmax(Message.emtpy).get(labelVariable).get
    def argmax(penalties: Message): State = {
      val states = domain.map(new SingletonState(labelVariable, _))
      val (state, s) = MathUtil.argmax(states, (s: State) => score(s))
      state
    }
    def features(state: State) = {
      val feats = contextFeatures conjoin labelFeatures(state.get(labelVariable).get)
      new GlobalParameterVector(module, feats)
    }
  }

  def classify(context: Context, observation: State): Label = model(context, observation).classify

}

trait Classifier[L, C] extends LocalClassifier with SourceModule {
  self =>

  type Label = L
  type Hidden = Variable[L]
  type ModelType = DefaultLocalModel
  type Context = C

  def labelFeatures(label: Label): ParameterVector
  def contextFeatures(context: Context): ParameterVector
  def variable(context: Context): Hidden
  def domain: Iterable[L]

  class DefaultLocalModel(val context: Context) extends LocalModel with SourceModel {
    val labelVariable = self.variable(context)
    val contextFeatures = self.contextFeatures(context)
    def labelFeatures(label: Label) = self.labelFeatures(label)
    val domain = self.domain
  }

  def model(c: Context, observed: State): ModelType = new DefaultLocalModel(c)

}


trait PipeableClassifier extends LocalClassifier {
  type Observed = Variable[ParameterVector]
  type ModelType <: PipeableLocalModel

  trait PipeableLocalModel extends LocalModel {
    def in: Variable[ParameterVector]
    def observed = Seq(in)
    val contextFeatures = observation.get(in).get
  }
}


class FeatureExtractor[C](val creator: C => Variable[ParameterVector],
                          val extractor: C => ParameterVector) extends SourceModule {
  type Hidden = Variable[ParameterVector]
  type Context = C
  type ModelType = ExtractorModel

  class ExtractorModel(val context: Context, val variable: Variable[ParameterVector]) extends SourceModel {
    lazy val cachedFeats = extractor(context)
    lazy val cachedArgmax = new SingletonState(variable, cachedFeats)
    def argmax(penalties: Message) = cachedArgmax
    def score(state: State) = if (state.get(variable).get == cachedFeats) 0.0 else Double.NegativeInfinity
    def hidden = Seq(variable)
  }

  def model(c: Context, observed: State) = new ExtractorModel(c, creator(c)) {}

}