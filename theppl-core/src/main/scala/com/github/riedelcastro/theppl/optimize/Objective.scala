package com.github.riedelcastro.theppl.optimize

import com.github.riedelcastro.theppl.util.HasLogger

/**
 * @author sriedel
 */
trait Objective extends OptimizableByValueAndGradient with HasLogger {

  case class GradientAndObjective(gradient: Array[Double], objective: Double)

  def optimizableValue = {
    updateGradientAndObjective()
    logger.info("Returning current objective to optimizer: " + gradientAndObjective.objective)
    gradientAndObjective.objective
  }

  def domainSize: Int
  def calculateGradientAndObjective(parameters: Array[Double]): GradientAndObjective

  private var gradientAndObjective: GradientAndObjective = null
  private var parameters: Array[Double] = Array.ofDim(domainSize)

  def numOptimizableParameters = domainSize
  def getOptimizableParameters(a: Array[Double]) {
    System.arraycopy(parameters, 0, a, 0, parameters.size)
  }
  def setOptimizableParameters(a: Array[Double]) {
    logger.info("Getting Parameters from optimizer")
    System.arraycopy(a, 0, parameters, 0, a.size)
    invalidate()
  }
  def optimizableParameter(index: Int) = parameters(index)

  def optimizableParameter_=(index: Int, d: Double) {
    parameters(index) = d
    invalidate()
  }

  def getOptimizableGradient(a: Array[Double]) {
    logger.info("Returning the current gradient to optimizer")
    updateGradientAndObjective()
    System.arraycopy(gradientAndObjective.gradient, 0, a, 0, a.size)
  }

  private var needUpdate = true
  def invalidate() {
    needUpdate = true
  }

  def updateGradientAndObjective() {

    if (needUpdate) {

      gradientAndObjective = calculateGradientAndObjective(parameters)
      logger.info("Obj:          " + gradientAndObjective.objective)
      needUpdate = false
    }
  }


}
