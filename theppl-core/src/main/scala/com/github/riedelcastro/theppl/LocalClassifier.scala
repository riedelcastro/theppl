package com.github.riedelcastro.theppl

import util.MathUtil
import java.io.{InputStream, OutputStream}
import Imports._

/**
 * todo:Find a better name (or remove class altogether)
 * @author sriedel
 */
trait LocalClassifier[Context] extends LinearLeafTemplate[Context] {
  template =>
  type LabelType
  type LabelVariableType <: Variable[LabelType]
  type PotentialType <: LocalPotential

  val weights = new ParameterVector()

  trait LocalPotential extends LinearPotential  {
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

  type PotentialType = DefaultLocalPotential
  def labelFeatures(label: LabelType): ParameterVector
  def contextFeatures(context: Context): ParameterVector
  def variable(context: Context): LabelVariableType
  def truth(context:Context): Option[LabelType] = None

  class DefaultLocalPotential(val context: Context)
    extends LocalPotential {
    val labelVariable = self.variable(context)
    val contextFeatures = self.contextFeatures(context)
    def labelFeatures(label: LabelType) = self.labelFeatures(label)
    override def truth = State(self.truth(context).map(labelVariable -> _).toMap)
  }

  def potential(c: Context): PotentialType = new DefaultLocalPotential(c)

}



