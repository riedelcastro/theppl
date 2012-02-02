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

  val weights = new ParameterVector()

  trait LocalModel extends LinearModel  {
    def labelFeatures(label: LabelType): ParameterVector
    def contextFeatures: ParameterVector
    def labelVariable: LabelVariableType
    def hidden = Seq(labelVariable)
    def features(state: State) = {
      val feats = contextFeatures conjoin labelFeatures(state.get(labelVariable).get)
      feats
    }
  }


}

trait Classifier[Context] extends LocalClassifier[Context] {
  self =>

  type ModelType = DefaultLocalModel
  def labelFeatures(label: LabelType): ParameterVector
  def contextFeatures(context: Context): ParameterVector
  def variable(context: Context): LabelVariableType

  class DefaultLocalModel(val context: Context)
    extends LocalModel {
    val labelVariable = self.variable(context)
    val contextFeatures = self.contextFeatures(context)
    def labelFeatures(label: LabelType) = self.labelFeatures(label)
  }

  def model(c: Context): ModelType = new DefaultLocalModel(c)

}



