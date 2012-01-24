package com.github.riedelcastro.theppl

import util.MathUtil
import java.io.{InputStream, OutputStream}
import Imports._

/**
 * @author sriedel
 */
trait LocalClassifier[Context] extends LinearLeafModule[Context] {
  module =>
  type LabelType
  type LabelVariableType <: Variable[LabelType]
  type ModelType <: LocalModel

  val weights = new HierarchicalParameterVector

  trait LocalModel extends LinearModel with FiniteSupportModel with BruteForceMarginalizer {
    def labelFeatures(label: LabelType): ParameterVector
    def contextFeatures: ParameterVector
    def labelVariable: LabelVariableType
    def hidden = Seq(labelVariable)
    def classify: LabelType = argmax(Message.empty).state.get(labelVariable).get
    def argmax(penalties: Message) = {
      val states = labelVariable.domain.map(new SingletonState(labelVariable, _))
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

  def classify(context: Context): LabelType = model(context).classify

}

trait Classifier[Context] extends LocalClassifier[Context] {
  self =>

  type ModelType = DefaultLocalModel
  def labelFeatures(label: LabelType): ParameterVector
  def contextFeatures(context: Context): ParameterVector
  def variable(context: Context): LabelVariableType

  class DefaultLocalModel(val context: Context)
    extends LocalModel with BruteForceExpectationCalculator {
    val labelVariable = self.variable(context)
    val contextFeatures = self.contextFeatures(context)
    def labelFeatures(label: LabelType) = self.labelFeatures(label)
  }

  def model(c: Context): ModelType = new DefaultLocalModel(c)

}



