package com.github.riedelcastro.theppl

/**
 * A factor graph representation of factorized models / sums.
 * @author sriedel
 */
trait FactorGraph {
  type SumType <: Sum

  type FactorType <: Factor
  type NodeType <: Node
  type EdgeType <: Edge

  trait Factor {
    def model: SumType#ArgType
    def edges: Iterable[EdgeType]
  }

  trait Edge {
    def node:NodeType
    def factor:FactorType
  }

  trait Node {
    def edges: Iterable[EdgeType]
  }

  def nodes: Iterable[NodeType]
  def factors: Iterable[FactorType]

}

