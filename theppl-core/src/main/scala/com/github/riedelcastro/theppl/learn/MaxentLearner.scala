package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl.optimize.{LimitedMemoryBFGS, Objective}
import com.github.riedelcastro.theppl.util.HasLogger
import com.github.riedelcastro.theppl._
import infer.Expectator
import java.io.PrintStream


/**
 * @author sriedel
 */
trait MaxentLearner[Context] extends Learner[Context] with SuperviseByExpectations[Context] with HasLogger {

  def iterations = 10

  val template: LinearTemplate[Context]

  case class Mapping(forward: Map[Any, Int], reverse: Map[Int, Any])

  def expectator(potential: template.PotentialType): Expectator

  def l2: Double = 0.0

  def train() {
    logger.info("Creating potentials.")
    val potentials = instances.map(i => i -> template.potential(i))

    logger.info("Extracting gold features.")
    val goldFeatures: Seq[ParameterVector] = potentials.map({case (context, potential) => targetExpectations(context, potential)})

    logger.info("Counting features.")
    val qualified = for (h <- goldFeatures.view;
                         feat <- h.values.keys) yield feat -> 1

    val byFeature = qualified.groupBy(_._1).mapValues(_.size)

    logger.info("Indexing.")
    val forward = byFeature.keys.zipWithIndex.toMap
    val backward = forward.map(_.swap)
    val mapping = Mapping(forward, backward)


    val llInstances = potentials.zip(goldFeatures).map {
      case ((i, m), f) => LLInstance(i, m, expectator(m), f)
    }
    val objective = new LLObjective(llInstances, mapping)
    val optimizer = new LimitedMemoryBFGS(objective)

    logger.info("Optimizing.")
    optimizer.optimize(iterations)
  }

  case class LLInstance(instance: Context, potential: template.PotentialType, expectator: Expectator, goldFeatures: ParameterVector)

  class LLObjective(instances: Seq[LLInstance], mapping: Mapping) extends Objective {
    def domainSize = mapping.forward.size
    def calculateGradientAndObjective(parameters: Array[Double]) = {

      //set weights
      for ((feat, index) <- mapping.forward) {
        template.weights(feat) = parameters(index)
      }

//      println("-----")
//      println(template.weights)

      //calculate gradient
      val gradient = new ParameterVector
      var objective = 0.0
      for (instance <- instances) {
        val expectations = instance.expectator.expectations()
        val goldFeatures = instance.goldFeatures
        val guessFeatures = expectations.featureExpectations
        gradient.add(goldFeatures, 1.0)
        gradient.add(guessFeatures, -1.0)
        objective += (goldFeatures dot template.weights) - expectations.logZ
//        println("****")
//        println(expectations.featureExpectations)
//        println(objective)

      }

//      println("Gradient: \n--------")
//      println(gradient)

      //L2 normalizer
      if (l2 != 0.0) {
        objective -= l2 * template.weights.norm2Sq
        gradient.add(template.weights.filterByKey(mapping.forward.keys), -2.0 * l2)
      }

      val log = new PrintStream("log/maxent")
      log.println("Gradient (regularized): \n--------")
      log.println(gradient)

      //convert
      val arrayGradient = Array.ofDim[Double](domainSize)
      for ((feat, index) <- mapping.forward) {
        arrayGradient(index) = gradient(feat)
      }

      GradientAndObjective(arrayGradient, objective)
    }
  }

}