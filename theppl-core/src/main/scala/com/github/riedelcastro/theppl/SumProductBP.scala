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
      node.edges.map(_.f2n).foldLeft[Message[Any]](penalties.message(node.variable))(_ + _)
    for (edge <- node.edges) {
      edge.n2f = node.belief - edge.f2n
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
    for (factor <- fg.factors) {
      expectations.add(factor.expectator.expectations(incomingMessages(factor)).featureExpectations, 1.0)
    }

    new Expectations {
      def featureExpectations = expectations
      def logMarginals = new Messages {
        val map: Map[Variable[Any], Message[Any]] = fg.nodes.map(n => n.variable -> n.belief.normalize).toMap
        def message[V](variable: Variable[V]) = map(variable).asInstanceOf[Message[V]]
      }
      def logZ = 0.0
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
  type FactorType = Factor
  type NodeType = Node
  type EdgeType = Edge
  def createFactor(potential: Model) = new Factor {
    def expectator = fg.expectator(potential)
  }
  def createNode(variable: Variable[Any]) = new Node {}
  def createEdge(potential: Model, variable: Variable[Any]) = new Edge {}
}

