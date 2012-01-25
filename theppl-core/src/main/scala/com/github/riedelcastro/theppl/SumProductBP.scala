package com.github.riedelcastro.theppl


/**
 * @author sriedel
 */
trait SumProductBP extends Expectator {

  val fg: MessagePassingFactorGraph

  def maxIterations:Int

  def incomingMessages(factor: fg.FactorType): Messages = {
    val incoming = new Messages {
      def message[V](variable: Variable[V]) =
        factor.edges.find(_.node.variable == variable).get.n2f.asInstanceOf[Message[V]]
    }
    incoming
  }

  def msgF2Vs(factor: fg.FactorType) {
    val incoming = incomingMessages(factor)
    val expectations = factor.expectator.expectations(incoming)
    for (edge <- factor.edges) {
      edge.f2n = expectations.logMarginals.message(edge.node.variable) - edge.n2f
    }
  }

  def msgV2Fs(node: fg.NodeType, penalties: Messages) {
    node.belief =
      node.edges.view.map(_.f2n).foldLeft[Message[Any]](penalties.message(node.variable))(_ + _).normalize
    for (edge <- node.edges) {
      edge.n2f = (node.belief - edge.f2n).normalize
    }
  }

  def expectations(penalties: Messages) = {
    //inititialize
    for (edge <- fg.edges) {
      edge.n2f = Message.empty(edge.node.variable)
      edge.f2n = Message.empty(edge.node.variable)
    }
    for (node <- fg.nodes) {
      node.belief = Message.empty(node.variable)
    }
    for (i <- 0 until maxIterations) {
      for (node <- fg.nodes) {
        msgV2Fs(node, penalties)
      }
      for (factor <- fg.factors) {
        msgF2Vs(factor)
      }
    }
    val expectations = new ParameterVector
    var logPartition = 0.0
    for (factor <- fg.factors) {
      val expPerFactor = factor.expectator.expectations(incomingMessages(factor))
      expectations.add(expPerFactor.featureExpectations, 1.0)
      logPartition += expPerFactor.logZ
    }
    //reduce double counts from nodes
    for (node <- fg.nodes) {
      logPartition -= (node.edges.size - 1) * node.belief.entropy
    }

    new Expectations {
      def featureExpectations = expectations
      def logMarginals = new Messages {
        val map: Map[Variable[Any], Message[Any]] = fg.nodes.map(n => n.variable -> n.belief).toMap
        def message[V](variable: Variable[V]) = map(variable).asInstanceOf[Message[V]]
      }
      def logZ = logPartition
    }
  }
}

object SumProductBPRecipe extends ExpectatorRecipe[SumModel] {
  def expectator(m: SumModel, cookbook: ExpectatorRecipe[Model] = DefaultExpectators) = {
    val factorGraph = new MessagePassingFactorGraph {
      def expectator(model: Model) = cookbook.expectator(model, cookbook)
    }
    factorGraph.add(m.args)
    new SumProductBP {
      val fg = factorGraph
      def maxIterations = 6
      def model = m
    }
  }
}


trait MessagePassingFactorGraph extends PotentialGraph {
  fg =>
  def expectator(model: Model): Expectator
  type FactorType = Factor
  type NodeType = Node
  type EdgeType = Edge
  trait Edge extends super.Edge {
    var n2f: Message[Any] = _
    var f2n: Message[Any] = _
  }
  trait Node extends super.Node {
    var belief: Message[Any] = _
  }
  trait Factor extends super.Factor {
    def expectator: Expectator
  }
  def createFactor(potential: Model) = new Factor {
    def expectator = fg.expectator(potential)
  }
  def createNode(variable: Variable[Any]) = new Node {}
  def createEdge(potential: Model, variable: Variable[Any]) = new Edge {}
}

