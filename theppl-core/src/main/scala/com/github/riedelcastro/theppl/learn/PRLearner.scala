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

  def targetExpectations(context: Context, model: q.ModelType): ParameterVector

  def train(trainData: Seq[Context]) {
    trait QplusP extends LinearModule[Context] {

      type ModelType = Overlay
      trait Overlay extends LinearModel with SumModel {
        def context: Context
        override def score(state: State) = super[SumModel].score(state)
        type ArgType = LinearModule[Context]#LinearModel
        lazy val qModel = q.model(context)
        lazy val pModel = p.model(context)
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
      def instances = trainData

    }

    //I-projection: train (q(w) + p_i-1) to match constraints of q. That is, train qPlusP based on q's target
    qPlusPLearner.train()

    //M-projection: train p_i to match q
    //p.train(trainData)
  }

}