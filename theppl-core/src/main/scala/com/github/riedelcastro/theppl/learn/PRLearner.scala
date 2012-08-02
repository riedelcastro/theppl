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

  val q: LinearTemplate[Context]
  val p: LinearTemplate[Context]

  def maxIterations: Int

  def targetExpectations(context: Context, potential: q.PotentialType): ParameterVector

  def instances: Seq[Context]

  def beta = 0.0
  def alpha = 0.0

  def maxQIterations = 10
  def maxPIterations = 10

  def train() {
    val qPlusP = new LinearTemplateWithBaseMeasure[Context] {
      type TemplateType = q.type
      type BaseType = p.type
      def template = q
      def base = p
    }

    val pPlusQ = new LinearTemplateWithBaseMeasure[Context] {
      type TemplateType = p.type
      type BaseType = q.type
      def template = p
      def base = q
    }

    val qPlusPLearner = new MaxentLearner[Context] {
      val template = qPlusP
      def instances = pr.instances
      def expectator(potential: template.PotentialType) = new SumProductBPRecipe(10).expectator(potential)
      def targetExpectations(context: Context, potential: template.PotentialType) = pr.targetExpectations(context, potential.potential)
      override def l2 = beta
      override def iterations = maxQIterations
    }


    val qPlusPLearners = for (group <- pr.instances.grouped(1).toSeq) yield new MaxentLearner[Context] {
      val template = qPlusP
      def instances = group
      def expectator(potential: template.PotentialType) = BruteForceExpectator.expectator(potential) //  new SumProductBPRecipe(20,0.000001).expectator(potential) //
      def targetExpectations(context: Context, potential: template.PotentialType) = pr.targetExpectations(context, potential.potential)
      override def l2 = beta
      override def iterations = maxQIterations
    }

//    val log = new PrintStream("log/pr.log")

    val pLearner = new MaxentLearner[Context] {
      val template = p
      def instances = pr.instances
      def expectator(potential: PotentialType) = Expectator(potential)
      def targetExpectations(context: Context, potential: PotentialType) = {
        val expectations = new SumProductBPRecipe().expectator(pPlusQ.potential(context)).expectations()
//        val qExpectations = qPlusP.potential(context).expectations
//        log.println("*****")
//        log.println(qExpectations.featureExpectations.values.map(pair => "%-20s %f %f".format(pair._1, pair._2, qPlusP.weights(pair._1))).mkString("\n"))
//        for (v <- potential.hidden) {
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
  val p: LinearTemplate[Context]
  val q: LinearTemplate[Context]

  def instances: Seq[Context]

  def targetExpectations(context: Context, potential: q.PotentialType): ParameterVector

  def beta = 0.0
  def alpha = 0.0

  def maxQIterations = 10
  def maxPIterations = 10

  def maxIterations: Int

  def pWeightKeys(context:Context):Iterable[Any]


  def train() {

    val qTrainer = new MaxentLearner[Context] {
      def instances = self.instances
      val template: q.type = q
      def expectator(potential: template.PotentialType) = potential.defaultExpectator()
      def targetExpectations(context: Context, potential: template.PotentialType) = self.targetExpectations(context, potential)
      override def iterations = maxQIterations
      override def l2 = beta
    }


    val pTrainer = new MaxentLearner[Context] {
      val template = p
      def instances = self.instances
      def expectator(potential: template.PotentialType) = potential.defaultExpectator()
      def targetExpectations(context: Context, potential: template.PotentialType) = {
        q.potential(context).expectations.featureExpectations.filterKeys(pWeightKeys(context))
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


trait LinearTemplateWithBaseMeasure[Context] extends LinearTemplate[Context] {
  self =>

  type TemplateType <: LinearTemplate[Context]
  type BaseType <: Template[Context]

  def template: TemplateType
  def base: BaseType

  type PotentialType = Overlay
  trait Overlay extends FeatureSumPotential with LinearPotential {
    def context: Context
    lazy val potential = template.potential(context)
    lazy val basePotential = base.potential(context)

    def featureArgs = IndexedSeq(potential)
    def otherArgs = IndexedSeq(basePotential)
    override def score(state: State) = super[FeatureSumPotential].score(state)
  }
  def potential(c: Context) = new Overlay {
    def context = c
  }
  def weights = template.weights
}
