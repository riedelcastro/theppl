package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._


/**
 * PR regularization
 * @author sriedel
 */
trait PRLearner[Context] {

  val q: LinearModule[Context] with Supervisor
  val p: LinearModule[Context]

  def train(trainData: Seq[Context]) {
    trait QplusP extends LinearModule[Context] {

      type ModelType = Overlay
      trait Overlay extends LinearModel with SumModel {
        def context:Context
        def argmax(penalties: Messages) = null
        override def score(state: State) = super[SumModel].score(state)
        type ArgType = LinearModule[Context]#LinearModel
        lazy val qModel = q.model(context)
        lazy val pModel = p.model(context)
        lazy val args = IndexedSeq[ArgType](qModel,pModel)
        override def features(state: State) = qModel.features(state)
      }
      def model(c: Context) = new Overlay {
        def context = c
      }
      def weights = q.weights
    }

    val qPlusP = new QplusP with MaxentLearner[Context] {
      def expectator(model: ModelType) = SumProductBPRecipe.expectator(model)
      def target(model: ModelType) = q.target(model.qModel)
    }

    //I-projection: train (q(w) + p_i-1) to match constraints of q. That is, train qPlusP based on q's target
    qPlusP.train(trainData)

    //M-projection: train p_i to match q
    //p.train(trainData)
  }

}