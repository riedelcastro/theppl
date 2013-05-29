package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl._
import util.HasLogger


/**
 * @author sriedel
 */
trait SumProductBP extends Expectator with HasLogger {

  val fg: MessagePassingFactorGraphOld

  def maxIterations: Int

  def incomingMessages(factor: fg.FactorType): Messages = {
    val incoming = new Messages {
      def message[V](variable: Variable[V]) =
        factor.edges.find(_.node.variable == variable).get.n2f.asInstanceOf[Message[V]]
    }
    incoming
  }

  //todo: these operations should be generalized for (a) faster operation and (b) different mp algorithms.
  def msgF2Vs(factor: fg.FactorType) {
    val incoming = incomingMessages(factor)
    val expectations = factor.expectator.expectations(incoming)
    for (edge <- factor.edges) {
      edge.f2nOld = edge.f2n
      edge.f2n = (expectations.logMarginals.message(edge.node.variable) - edge.n2f)
    }
  }
  
  def maxResidual():Double = {
    fg.edges.view.map(e => (e.f2n - e.f2nOld).norm1).max
  }

  def msgV2Fs(node: fg.NodeType, penalties: Messages) {
    node.belief =
      node.edges.view.map(_.f2n).foldLeft[Message[Any]](penalties.message(node.variable))(_ + _).normalize.materialize
    for (edge <- node.edges) {
      edge.n2f = (node.belief - edge.f2n).normalize.materialize
    }
  }

  def residualThreshold = 0.001

  def calculateMarginals(penalties: Messages) {
    for (edge <- fg.edges) {
      edge.n2f = Message.empty(edge.node.variable)
      edge.f2n = Message.empty(edge.node.variable)
      edge.f2nOld = Message.empty(edge.node.variable)
    }
    for (node <- fg.nodes) {
      node.belief = Message.empty(node.variable)
    }
    var index = 0
    var res = Double.PositiveInfinity
    do {
      for (node <- fg.nodes) {
        msgV2Fs(node, penalties)
      }
      for (factor <- fg.factors) {
        msgF2Vs(factor)
      }
      index += 1
      res = maxResidual()
      logger.trace(lazyString("Max residual at iteration %d: %f".format(index,res)))
    } while (index < maxIterations && res > residualThreshold)
  }
  def outgoingMarginals: Messages = {
    new Messages {
      val map: Map[Variable[Any], Message[Any]] = fg.nodes.map(n => n.variable -> n.belief).toMap
      def message[V](variable: Variable[V]) = map(variable).asInstanceOf[Message[V]]
    }
  }

  override def marginalize(penalties: Messages) = {
    calculateMarginals(penalties)
    new MarginalizeResult {
      def logMarginals = outgoingMarginals
      def logZ = logPartitionFunction
    }
  }

  def logPartitionFunction = {
    var logPartition = 0.0
    for (factor <- fg.factors) {
      val margResult = factor.expectator.marginalize(incomingMessages(factor))
      logPartition += margResult.logZ
      for (edge <- factor.edges) {
        logPartition -= (edge.node.belief.map(math.exp(_)) dot edge.n2f)
      }
    }
    //reduce entropy double counts from nodes
    for (node <- fg.nodes) {
      logPartition -= (node.edges.size - 1) * node.belief.entropy
    }
    logPartition
  }

  def expectations(penalties: Messages) = {
    calculateMarginals(penalties)

    val expectations = new ParameterVector
    for (factor <- fg.featureFactors) {
      val expPerFactor = factor.expectator.expectations(incomingMessages(factor))
      expectations.add(expPerFactor.featureExpectations, 1.0)
    }

    new Expectations {
      def featureExpectations = expectations
      def logMarginals = outgoingMarginals
      def logZ = logPartitionFunction
    }
  }
}

class SumProductBPRecipe(iterations:Int = 6, residual:Double = 0.001) extends ExpectatorRecipe[FeatureSumPotential] {
  def expectator(m: FeatureSumPotential, cookbook: ExpectatorRecipe[Potential] = Expectator) = {
    val factorGraph = new MessagePassingFactorGraphOld {
      def expectator(potential: Potential) = cookbook.expectator(potential, cookbook)
    }
    factorGraph.add(m.otherArgs, m.featureArgs)
    new SumProductBP {
      val fg = factorGraph
      val potential = m
      def maxIterations = iterations
      override def residualThreshold = residual
    }
  }
}
object SumProductBPRecipe extends SumProductBPRecipe(10, 0.001)


trait MessagePassingFactorGraphOld extends PotentialGraphOld {
  fg =>
  def expectator(potential: Potential): Expectator
  type FactorType = OtherFactor
  type FeatureFactorType = FeatureFactor
  type NodeType = Node
  type EdgeType = Edge
  trait Edge extends super.Edge {
    var n2f: Message[Any] = _
    var f2n: Message[Any] = _
    var f2nOld: Message[Any] = _
  }
  trait Node extends super.Node {
    var belief: Message[Any] = _
  }

  trait OtherFactor extends super.Factor {
    def expectator: Expectator
  }

  trait FeatureFactor extends OtherFactor with super.FeatureFactor {
  }


  trait Factor extends super.Factor {
    def marginalizer: Marginalizer
  }

  def createFeatureFactor(potential: Potential) = new FeatureFactor {
    def expectator = fg.expectator(potential)
  }

  def createFactor(potential: Potential) = new OtherFactor {
    def expectator = fg.expectator(potential)
  }
  def createNode(variable: Variable[Any]) = new Node {}
  def createEdge(potential: Potential, variable: Variable[Any]) = new Edge {}
}

