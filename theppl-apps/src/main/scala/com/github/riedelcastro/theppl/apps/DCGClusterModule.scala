package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl.util.CollectionUtil
import com.github.riedelcastro.theppl.{Potential, State, Variable, Template}


/**
 * A clustering template that uses delayed column generation for inference
 * @author sriedel
 */
trait DCGClusterTemplate[Context] extends Template[Context] {
  thisTemplate =>

  type Instance
  type PotentialType <: DCGClusterPotential with Potential { type Instance = thisTemplate.Instance}
  type SlaveTemplate <: Template[Context]
  def slaveTemplate:SlaveTemplate

}

/**
 * A clustering potential that uses a slave potential to score each cluster in isolation,
 * and that enforces consistency between active clusters. Argmaxing
 * is done through delayed column generation.
 */
trait DCGClusterPotential extends Potential {
  type Instance
  type SlavePotential <: Potential
  def instances:Seq[Instance]
  def slavePotential:SlavePotential

  /**
   * Defines the variables to use for representing a cluster. These variables
   * are used in the master problem
   */
  def clusterVariable(instances: Set[Instance]): Variable[Boolean]

  /**
   * Defines the variables to represent whether a single mention is a member of a cluster.
   * These variables are used in the slave problem.
   */
  def instanceVariable(instance: Instance): Variable[Boolean]

  def hidden = CollectionUtil.allSubSequences(instances).map(mentions => clusterVariable(mentions.toSet))


}