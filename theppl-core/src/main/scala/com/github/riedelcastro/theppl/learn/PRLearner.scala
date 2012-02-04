package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._


/**
 * PR regularization
 * @author sriedel
 */
trait PRLearner[Context] {
  pr =>

  val q: LinearModule[Context]
  val p: LinearModule[Context]

  def maxIterations: Int

  def targetExpectations(context: Context, model: q.ModelType): ParameterVector

  def instances: Seq[Context]
  
  def beta = 0.0

  def train() {
    val qPlusP = new LinearModuleWithBaseMeasure[Context] {
      type ModuleType = q.type
      type BaseType = p.type
      def module = q
      def base = p
    }

    val pPlusQ = new LinearModuleWithBaseMeasure[Context] {
      type ModuleType = p.type
      type BaseType = q.type
      def module = p
      def base = q
    }

    val qPlusPLearner = new MaxentLearner[Context] {
      val module = qPlusP
      def expectator(model: module.ModelType) = SumProductBPRecipe.expectator(model)
      def targetExpectations(context: Context, model: module.ModelType) = pr.targetExpectations(context, model.model)
      def instances = pr.instances
      override def l2 = beta
    }



    val pLearner = new MaxentLearner[Context] {
      val module = p
      def expectator(model: ModelType) = Expectator(model)
      def targetExpectations(context: Context, model: ModelType) =
        Expectator(pPlusQ.model(context)).expectations().featureExpectations
      def instances = pr.instances
    }

    for (iteration <- 0 until maxIterations) {
      //I-projection: train (q(w) + p_i-1) to match constraints of q. That is, train qPlusP based on q's target
      qPlusPLearner.train()

      //M-projection: train p_i to match q
      pLearner.train()
    }
  }

}

trait LinearModuleWithBaseMeasure[Context] extends LinearModule[Context] {
  self =>

  type ModuleType <: LinearModule[Context]
  type BaseType <: Module[Context]

  def module:ModuleType
  def base:BaseType

  type ModelType = Overlay
  trait Overlay extends FeatureSumModel with LinearModel {
    def context: Context
    lazy val model = module.model(context)
    lazy val baseModel = base.model(context)

    def featureArgs = IndexedSeq(model)
    def otherArgs = IndexedSeq(baseModel)
    override def score(state: State) = super[FeatureSumModel].score(state)
  }
  def model(c: Context) = new Overlay {
    def context = c
  }
  def weights = module.weights
}
