package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl.optimize.{LimitedMemoryBFGS, Objective}
import com.github.riedelcastro.theppl.util.HasLogger
import com.github.riedelcastro.theppl._


/**
 * @author sriedel
 */
trait MaxentLearner[Context] extends Learner[Context] with SuperviseByExpectations[Context] with HasLogger {

  var iterations = 10

  val module: LinearModule[Context]

  case class Mapping(forward: Map[Feat, Int], reverse: Map[Int, Feat])

  def expectator(model: module.ModelType): Expectator

  def train() {
    logger.info("Creating models.")
    val models = instances.map(i => i -> module.model(i))

    logger.info("Extracting gold features.")
    val goldFeatures: Seq[ParameterVector] = models.map({case (context, model) => targetExpectations(context, model)})

    logger.info("Counting features.")
    val qualified = for (h <- goldFeatures.view;
                         feat <- h.values.keys) yield feat -> 1

    val byFeature = qualified.groupBy(_._1).mapValues(_.size)

    logger.info("Indexing.")
    val forward = byFeature.keys.zipWithIndex.toMap
    val backward = forward.map(_.swap)
    val mapping = Mapping(forward, backward)


    val llInstances = models.zip(goldFeatures).map {
      case ((i, m), f) => LLInstance(i, m, expectator(m), f)
    }
    val objective = new LLObjective(llInstances, mapping)
    val optimizer = new LimitedMemoryBFGS(objective)

    logger.info("Optimizing.")
    optimizer.optimize(iterations)
  }

  case class LLInstance(instance: Context, model: module.ModelType, expectator: Expectator, goldFeatures: ParameterVector)

  class LLObjective(instances: Seq[LLInstance], mapping: Mapping) extends Objective {
    def domainSize = mapping.forward.size
    def calculateGradientAndObjective(parameters: Array[Double]) = {

      //set weights
      for ((feat, index) <- mapping.forward) {
        module.weights(feat) = parameters(index)
      }

      //calculate gradient
      val gradient = new ParameterVector
      var objective = 0.0
      for (instance <- instances) {
        val expectations = instance.expectator.expectations(Messages.empty)
        val goldFeatures = instance.goldFeatures
        gradient.add(goldFeatures, 1.0)
        gradient.add(expectations.featureExpectations, -1.0)
        objective += (goldFeatures dot module.weights) - expectations.logZ
      }

      //convert
      val arrayGradient = Array.ofDim[Double](domainSize)
      for ((feat, index) <- mapping.forward) {
        arrayGradient(index) = gradient(feat)
      }

      GradientAndObjective(arrayGradient, objective)
    }
  }

}