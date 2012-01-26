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
  
  def instances:Seq[Context]

  def train() {
    trait QplusP extends LinearModule[Context] {

      type ModelType = Overlay
      trait Overlay extends LinearModel with SumModel {
        def context: Context
        override def score(state: State) = super[SumModel].score(state)
        type ArgType = com.github.riedelcastro.theppl.LinearModel
        lazy val qModel = q.model(context)
        lazy val pModel = new com.github.riedelcastro.theppl.LinearModel with FiniteSupportModel {
          val self = p.model(context)
          val weights = new ParameterVector()
          def features(state: State) = new ParameterVector()
          def hidden = self.hidden
          override def score(state: State) = self.score(state)
        }
        lazy val args = IndexedSeq[ArgType](qModel, pModel)
        override def features(state: State) = qModel.features(state)
      }
      def model(c: Context) = new Overlay {
        def context = c
      }
      def weights = q.weights
    }

    val qPlusP = new QplusP {}
    val qPlusPLearner = new MaxentLearner[Context] {
      val module = qPlusP
      def expectator(model: module.ModelType) = SumProductBPRecipe.expectator(model)
      def targetExpectations(context: Context, model: module.ModelType) = pr.targetExpectations(context, model.qModel)
      def instances = pr.instances
    }

    val pLearner = new MaxentLearner[Context] {
      val module = p
      def expectator(model: ModelType) = Expectator(model)
      def targetExpectations(context: Context, model: ModelType) =
        Expectator(qPlusP.model(context)).expectations().featureExpectations
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