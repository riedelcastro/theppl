package com.github.riedelcastro.theppl.infer

import collection.mutable.ArrayBuffer
import com.github.riedelcastro.theppl.{FeatureModel, Model, Variable}

/**
 * A factor graph representation of factorized models / sums.
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
    var potential: Model = _
  }
  trait FeatureFactor extends super.Factor {
  }

  val featureFactors = new ArrayBuffer[FeatureFactorType]


  type NodeType <: Node
  type FactorType <: Factor
  type FeatureFactorType <: FeatureFactor with FactorType

  def createFactor(potential: Model): FactorType
  def createFeatureFactor(potential: Model): FeatureFactorType
  def createNode(variable: Variable[Any]): NodeType
  def createEdge(potential: Model, variable: Variable[Any]): EdgeType

  def add(otherPotentials: Iterable[Model], featurePotentials: Iterable[FeatureModel] = Seq.empty) {
    val potentials = otherPotentials ++ featurePotentials
    val varModelPairs = potentials.flatMap(a => a.hidden.map(_ -> a))
    val var2model = varModelPairs.groupBy(_._1)
    val var2node = var2model.keys.map(v => v -> {
      val node = createNode(v)
      node.variable = v
      node
    }).toMap
    val otherModel2factor = otherPotentials.map(p => p -> {
      val factor = createFactor(p)
      factor.potential = p
      factor
    })
    val featureModel2factor = featurePotentials.map(p => p -> {
      val factor = createFeatureFactor(p)
      factor.potential = p
      factor
    })
    val model2factor = (otherModel2factor ++ featureModel2factor).toMap

    val edges = for ((v, m) <- varModelPairs) yield {
      val node = var2node(v)
      val factor = model2factor(m)
      val edge = createEdge(m, v)
      node.edges += edge
      factor.edges += edge
      edge.node = node
      edge.factor = factor
      edge
    }
    this.factors ++= model2factor.values
    this.featureFactors ++= featureModel2factor.map(_._2)
    this.nodes ++= var2node.values
    this.edges ++= edges

  }

}

