package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import infer.{SumProductBP, BruteForceExpectator, SumProductBPRecipe, Expectator}
import util.HasLogger
import java.io.PrintStream


/**
 * PR regularization
 * @author sriedel
 */
trait PRLearner[Context] extends HasLogger {
  pr =>

  val q: LinearModule[Context]
  val p: LinearModule[Context]

  def maxIterations: Int

  def targetExpectations(context: Context, model: q.ModelType): ParameterVector

  def instances: Seq[Context]

  def beta = 0.0
  def alpha = 0.0

  def maxQIterations = 10
  def maxPIterations = 10

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
      def instances = pr.instances
      def expectator(model: module.ModelType) = new SumProductBPRecipe(10).expectator(model)
      def targetExpectations(context: Context, model: module.ModelType) = pr.targetExpectations(context, model.model)
      override def l2 = beta
      override def iterations = maxQIterations
    }


    val qPlusPLearners = for (group <- pr.instances.grouped(1).toSeq) yield new MaxentLearner[Context] {
      val module = qPlusP
      def instances = group
      def expectator(model: module.ModelType) = BruteForceExpectator.expectator(model) //  new SumProductBPRecipe(20,0.000001).expectator(model) //
      def targetExpectations(context: Context, model: module.ModelType) = pr.targetExpectations(context, model.model)
      override def l2 = beta
      override def iterations = maxQIterations
    }

//    val log = new PrintStream("log/pr.log")

    val pLearner = new MaxentLearner[Context] {
      val module = p
      def instances = pr.instances
      def expectator(model: ModelType) = Expectator(model)
      def targetExpectations(context: Context, model: ModelType) = {
        val expectations = new SumProductBPRecipe().expectator(pPlusQ.model(context)).expectations()
//        val qExpectations = qPlusP.model(context).expectations
//        log.println("*****")
//        log.println(qExpectations.featureExpectations.values.map(pair => "%-20s %f %f".format(pair._1, pair._2, qPlusP.weights(pair._1))).mkString("\n"))
//        for (v <- model.hidden) {
//          log.println(v + "\n" + expectations.logMarginals.message(v).exp)
//        }
        expectations.featureExpectations
      }
      override def l2 = alpha
      override def iterations = maxPIterations

    }

    for (iteration <- 0 until maxIterations) {
      logger.info("PR iteration " + iteration)
      //I-projection: train (q(w) + p_i-1) to match constraints of q. That is, train qPlusP based on q's target
      //qPlusPLearner.train()
      logger.info("I-Projection")
      qPlusPLearners.foreach(_.train())

      //M-projection: train p_i to match q
      logger.info("M-Projection")
      pLearner.train()
    }
  }

}

trait SimplePRLearner[Context] extends HasLogger {
  self =>
  val p: LinearModule[Context]
  val q: LinearModule[Context]

  def instances: Seq[Context]

  def targetExpectations(context: Context, model: q.ModelType): ParameterVector

  def beta = 0.0
  def alpha = 0.0

  def maxQIterations = 10
  def maxPIterations = 10

  def maxIterations: Int

  def pWeightKeys(context:Context):Iterable[Any]


  def train() {

    val qTrainer = new MaxentLearner[Context] {
      def instances = self.instances
      val module: q.type = q
      def expectator(model: module.ModelType) = model.defaultExpectator()
      def targetExpectations(context: Context, model: module.ModelType) = self.targetExpectations(context, model)
      override def iterations = maxQIterations
      override def l2 = beta
    }


    val pTrainer = new MaxentLearner[Context] {
      val module = p
      def instances = self.instances
      def expectator(model: module.ModelType) = model.defaultExpectator()
      def targetExpectations(context: Context, model: module.ModelType) = {
        q.model(context).expectations.featureExpectations.filterByKey(pWeightKeys(context))
      }
      override def iterations = maxPIterations
      override def l2 = alpha
    }

    for (iteration <- 0 until maxIterations) {
      logger.info("PR iteration " + iteration)

      logger.info("I-Projection")
      //copy weights from p to qPlusP
      for (key <- p.weights.keys) q.weights(key) = p.weights(key)
      //train q.
      qTrainer.train()

      //M-projection: train p_i to match q
      logger.info("M-Projection")
      pTrainer.train()
    }
  }

}


trait LinearModuleWithBaseMeasure[Context] extends LinearModule[Context] {
  self =>

  type ModuleType <: LinearModule[Context]
  type BaseType <: Module[Context]

  def module: ModuleType
  def base: BaseType

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
