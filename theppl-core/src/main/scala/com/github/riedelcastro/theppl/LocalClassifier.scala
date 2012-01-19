package com.github.riedelcastro.theppl

import util.MathUtil
import java.io.{InputStream, OutputStream}
import Imports._

/**
 * @author sriedel
 */
trait LocalClassifier extends LinearLeafModule {
  module =>
  type Label
  type ModelType <: LocalModel

  val weights = new GlobalParameterVector

  trait LocalModel extends LinearModel with FiniteSupportModel with BruteForceMarginalizer {
    def labelFeatures(label: Label): ParameterVector
    def contextFeatures: ParameterVector
    def labelVariable: Variable[Label]
    def domain: Iterable[Label]
    def restrictions = Seq(Restriction(labelVariable,domain))
    def observation: State
    def classify: Label = argmax(Message.emtpy).state.get(labelVariable).get
    def argmax(penalties: Message) = {
      val states = domain.map(new SingletonState(labelVariable, _))
      val (st, sc) = MathUtil.argmax(states, (s: State) => score(s))
      new ArgmaxResult {
        def score = sc
        def state = st
      }
    }
    def features(state: State) = {
      val feats = contextFeatures conjoin labelFeatures(state.get(labelVariable).get)
      new GlobalParameterVector(module, feats)
    }
  }

  def classify(context: Context, observation: State): Label = model(context, observation).classify

}

trait ClassifierOld[L, C] extends LocalClassifier with SourceModule {
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

/**
 * A Local Classifier.
 *
 * @param v mapping from contexts to a variable.
 * @param dom the possible labels the variable can take on.
 * @param cf feature function for the context.
 * @param lf feature function for the label.
 */
case class Classifier[L, C](v: C => Variable[L],
                               dom: Seq[L],
                               cf: C => ParameterVector,
                               lf: L => ParameterVector = (l: L) => Feat(l)) extends ClassifierOld[L, C] {
  val domain = dom
  def labelFeatures(label: Label) = lf(label)
  def contextFeatures(context: Context) = cf(context)
  def variable(context: Context) = v(context)
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


trait NoSerialization extends Module {
  def load(in: InputStream) = {}
  def save(out: OutputStream) = {}
}