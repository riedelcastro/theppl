package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl.optimize.{LimitedMemoryBFGS, Objective}
import com.github.riedelcastro.theppl.util.HasLogger
import com.github.riedelcastro.theppl.{Feat, HierarchicalParameterVector, Message, LinearModule}


/**
 * @author sriedel
 */
trait MaxentLearner[Context] extends LinearModule[Context] with Learner[Context] with HasLogger {

  var iterations = 10

  case class Mapping(forward:Map[(Seq[Any],Feat),Int],reverse:Map[Int,(Seq[Any],Feat)])
  
  def train(instances: Seq[Context]) {
    logger.info("Creating models.")
    val models = instances.map(model(_))

    logger.info("Extracting gold features.")
    val goldFeatures: Seq[HierarchicalParameterVector] = models.map(m => m.features(target(m)))

    logger.info("Counting features.")
    val qualified = for (h <- goldFeatures.view;
                         (path, feats) <- h.params;
                         feat <- feats.values.keys) yield (path,feat) -> 1
    val byFeature = qualified.groupBy(_._1).mapValues(_.size)
    
    logger.info("Indexing.")
    val forward = byFeature.keys.zipWithIndex.toMap
    val backward = forward.map(_.swap)
    val mapping = Mapping(forward,backward)
    

    val llInstances = instances.zip(models).zip(goldFeatures).map{
      case ((i,m),f) => LLInstance(i,m,f)
    }
    val objective = new LLObjective(llInstances, mapping)
    val optimizer = new LimitedMemoryBFGS(objective)

    logger.info("Optimizing.")
    optimizer.optimize(iterations)
  }

  case class LLInstance(instance:Context,model:ModelType,goldFeatures:HierarchicalParameterVector)

  class LLObjective(instances: Seq[LLInstance],mapping:Mapping) extends Objective {
    def domainSize = mapping.forward.size
    def calculateGradientAndObjective(parameters: Array[Double]) = {

      //set weights
      for (((path,feat),index) <- mapping.forward) {
        weights(path,feat) = parameters(index)
      }

      //calculate gradient
      val gradient = new HierarchicalParameterVector
      var objective = 0.0
      for (instance <- instances) {
        val expectations = instance.model.expectations(Message.empty)
        val goldFeatures = instance.goldFeatures 
        gradient.add(goldFeatures, 1.0)
        gradient.add(expectations.featureExpectations, -1.0)
        objective += (goldFeatures dot weights) - expectations.logZ
      }

      //convert
      val arrayGradient = Array.ofDim[Double](domainSize)
      for (((path,feat),index) <- mapping.forward) {
        arrayGradient(index) = gradient(path,feat)
      }

      GradientAndObjective(arrayGradient, objective)
    }
  }

}