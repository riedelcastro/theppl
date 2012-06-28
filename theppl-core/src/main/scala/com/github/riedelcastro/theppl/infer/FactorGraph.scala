package com.github.riedelcastro.theppl.infer

import collection.mutable.ArrayBuffer
import com.github.riedelcastro.theppl.{FeaturePotential, Potential, Variable}

/**
 * A factor graph representation of factorized potentials / sums.
 * @author sriedel
 */
trait FactorGraph {

  type FactorType <: Factor
  type NodeType <: Node
  type EdgeType <: Edge

  trait Factor {
    def edges: IndexedSeq[EdgeType]
  }

  trait Edge {
    def node: NodeType
    def factor: FactorType
  }

  trait Node {
    def edges: IndexedSeq[EdgeType]
  }

  def nodes: IndexedSeq[NodeType]
  def factors: IndexedSeq[FactorType]
  def edges: IndexedSeq[EdgeType]


}

trait MutableFactorGraph extends FactorGraph {
  val nodes = new ArrayBuffer[NodeType]
  val factors = new ArrayBuffer[FactorType]
  val edges = new ArrayBuffer[EdgeType]


  type FactorType <: Factor
  type NodeType <: Node
  type EdgeType <: Edge

  trait Factor extends super.Factor {
    val edges = new ArrayBuffer[EdgeType]
  }
  trait Node extends super.Node {
    val edges = new ArrayBuffer[EdgeType]
  }

  trait Edge extends super.Edge {
    var node: NodeType = _
    var factor: FactorType = _
  }

}

trait PotentialGraph extends MutableFactorGraph {

  trait Node extends super.Node {
    var variable: Variable[Any] = _
  }
  trait Factor extends super.Factor {
    var potential: Potential = _
  }
  trait FeatureFactor extends super.Factor {
  }

  val featureFactors = new ArrayBuffer[FeatureFactorType]


  type NodeType <: Node
  type FactorType <: Factor
  type FeatureFactorType <: FeatureFactor with FactorType

  def createFactor(potential: Potential): FactorType
  def createFeatureFactor(potential: Potential): FeatureFactorType
  def createNode(variable: Variable[Any]): NodeType
  def createEdge(potential: Potential, variable: Variable[Any]): EdgeType

  def add(otherPotentials: Iterable[Potential], featurePotentials: Iterable[FeaturePotential] = Seq.empty) {
    val potentials = otherPotentials ++ featurePotentials
    val varPotentialPairs = potentials.flatMap(a => a.hidden.map(_ -> a))
    val var2potential = varPotentialPairs.groupBy(_._1)
    val var2node = var2potential.keys.map(v => v -> {
      val node = createNode(v)
      node.variable = v
      node
    }).toMap
    val otherPotential2factor = otherPotentials.map(p => p -> {
      val factor = createFactor(p)
      factor.potential = p
      factor
    })
    val featurePotential2factor = featurePotentials.map(p => p -> {
      val factor = createFeatureFactor(p)
      factor.potential = p
      factor
    })
    val potential2factor = (otherPotential2factor ++ featurePotential2factor).toMap

    val edges = for ((v, m) <- varPotentialPairs) yield {
      val node = var2node(v)
      val factor = potential2factor(m)
      val edge = createEdge(m, v)
      node.edges += edge
      factor.edges += edge
      edge.node = node
      edge.factor = factor
      edge
    }
    this.factors ++= potential2factor.values
    this.featureFactors ++= featurePotential2factor.map(_._2)
    this.nodes ++= var2node.values
    this.edges ++= edges

  }

}

