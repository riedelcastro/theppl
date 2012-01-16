package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl.util.StreamUtil
import com.github.riedelcastro.theppl.{Model, State, Variable, Module}


/**
 * A clustering module that uses delayed column generation for inference
 * @author sriedel
 */
trait DCGClusterModule extends Module {
  thisModule =>

  type Instance
  type ModelType <: DCGClusterModel with Model { type Instance = thisModule.Instance}
  type SlaveModule <: Module { type Context = thisModule.Context }
  def slaveModule:SlaveModule

}

/**
 * A clustering model that uses a slave model to score each cluster in isolation,
 * and that enforces consistency between active clusters. Argmaxing
 * is done through delayed column generation.
 */
trait DCGClusterModel extends Model {
  type Instance
  type SlaveModel <: Model
  def instances:Seq[Instance]
  def slaveModel:SlaveModel

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

  def hidden = StreamUtil.allSubSequences(instances).map(mentions => clusterVariable(mentions.toSet))


}