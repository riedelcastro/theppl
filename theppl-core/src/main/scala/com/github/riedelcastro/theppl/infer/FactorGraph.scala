package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.{Potential, Variable}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
class FactorGraph[N, E, F](terms: Seq[Potential],
                           factorContent: Potential => F,
                           nodeContent: Variable[Any] => N,
                           edgeContent: (Potential, Variable[Any]) => E) {


  class Node(val variable: Variable[Any], val content: N) {
    val edges = new ArrayBuffer[Edge]
  }

  class Edge(val factor: Factor, val node: Node, val content: E)

  class Factor(val term: Potential, val content: F) {
    val edges = new ArrayBuffer[Edge]
  }

  val term2Factor = new mutable.HashMap[Potential, Factor]
  val variable2Node = new mutable.HashMap[Variable[Any], Node]
  val factors = new ArrayBuffer[Factor]
  val edges = new ArrayBuffer[Edge]
  def nodes = variable2Node.values
  for (term <- terms) {
    val factor = new Factor(term, factorContent(term))
    for (v <- term.variables) {
      val node = variable2Node.getOrElseUpdate(v, new Node(v, nodeContent(v)))
      val edge = new Edge(factor, node, edgeContent(term, v))
      factor.edges += edge
      node.edges += edge
      edges += edge
    }
    factors += factor
    term2Factor(term) = factor
  }
}

trait GenericFactorGraph {
  type PotentialType <: Potential
  type VariableType <: Variable[Any]
  type FactorContentType
  type EdgeContentType
  type NodeContentType

  def terms:Seq[PotentialType]
  def createFactorContent(potential:PotentialType):FactorContentType
  def createNodeContent(variable:VariableType):NodeContentType
  def createEdgeContent(potential:PotentialType,variable:VariableType):EdgeContentType
  def variables(potential:PotentialType):Seq[VariableType]

  class Node(val variable: VariableType, val content: NodeContentType) {
    val edges = new ArrayBuffer[Edge]
  }

  class Edge(val factor: Factor, val node: Node, val content: EdgeContentType)

  class Factor(val term: PotentialType, val content: FactorContentType) {
    val edges = new ArrayBuffer[Edge]
  }

  val term2Factor = new mutable.HashMap[PotentialType, Factor]
  val variable2Node = new mutable.HashMap[VariableType, Node]
  val factors = new ArrayBuffer[Factor]
  val edges = new ArrayBuffer[Edge]
  def nodes = variable2Node.values
  for (term <- terms) {
    val factor = new Factor(term, createFactorContent(term))
    for (v <- variables(term)) {
      val node = variable2Node.getOrElseUpdate(v, new Node(v, createNodeContent(v)))
      val edge = new Edge(factor, node, createEdgeContent(term, v))
      factor.edges += edge
      node.edges += edge
      edges += edge
    }
    factors += factor
    term2Factor(term) = factor
  }


}
