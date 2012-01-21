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

  val weights = new HierarchicalParameterVector

  trait LocalModel extends LinearModel with FiniteSupportModel with BruteForceMarginalizer {
    def labelFeatures(label: Label): ParameterVector
    def contextFeatures: ParameterVector
    def labelVariable: Variable[Label]
    def domain: Iterable[Label]
    def restrictions = Seq(Restriction(labelVariable,domain))
    def classify: Label = argmax(Message.empty).state.get(labelVariable).get
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
      new HierarchicalParameterVector(module, feats)
    }
  }

  def classify(context: Context): Label = model(context).classify

}

trait ClassifierOld[L, C] extends LocalClassifier {
  self =>

  type Label = L
  type Hidden = Variable[L]
  type ModelType = DefaultLocalModel
  type Context = C

  def labelFeatures(label: Label): ParameterVector
  def contextFeatures(context: Context): ParameterVector
  def variable(context: Context): Hidden
  def domain: Iterable[L]

  class DefaultLocalModel(val context: Context)
    extends LocalModel with BruteForceExpectationCalculator{
    val labelVariable = self.variable(context)
    val contextFeatures = self.contextFeatures(context)
    def labelFeatures(label: Label) = self.labelFeatures(label)
    val domain = self.domain
  }

  def model(c: Context): ModelType = new DefaultLocalModel(c)

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

