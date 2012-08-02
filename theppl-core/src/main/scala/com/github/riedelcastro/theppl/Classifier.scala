package com.github.riedelcastro.theppl

import util.MathUtil
import java.io.{InputStream, OutputStream}
import Imports._

/**
 * A Local Classifier.
 * @author sriedel
 */
trait Classifier[Context] extends LinearLeafTemplate[Context] {
  self =>

  type LabelType
  type LabelVariableType <: Variable[LabelType]
  type PotentialType = ClassifierPotential
  def labelFeatures(label: LabelType): ParameterVector
  def contextFeatures(context: Context): ParameterVector
  def variable(context: Context): LabelVariableType
  def truth(context: Context): Option[LabelType] = None

  val weights = new ParameterVector()

  class ClassifierPotential(val context: Context) extends LinearPotential {
    val labelVariable = self.variable(context)
    val contextFeatures = self.contextFeatures(context)
    def labelFeatures(label: LabelType) = self.labelFeatures(label)
    def hidden = Seq(labelVariable)
    def features(state: State) = {
      val feats = contextFeatures conjoin labelFeatures(state.get(labelVariable).get)
      feats
    }

    override def truth = State(self.truth(context).map(labelVariable -> _).toMap)
  }

  def potential(c: Context): PotentialType = new ClassifierPotential(c)

}



