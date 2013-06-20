package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.term.{TermImplicits, Unroller}
import com.github.riedelcastro.theppl._

/**
 * @author Sebastian Riedel
 */
trait GenericMessagePassing {

  type MessageCalculator

  class FactorContent(val calculator: MessageCalculator)
  class EdgeContent(var f2n: Message[Any], var n2f: Message[Any])
  class NodeContent(var belief: Message[Any])

  /**
   * Returns for a given potential the message calculator (could be argmaxer, marginalizer, max-marginalizer etc.)
   * @param potential the potential for which the message calculator is returned
   * @return the message calculator.
   */
  def messageCalculator(potential: Potential): MessageCalculator

  /**
   * The potentials on the network factors.
   */
  def potentials: Seq[Potential]

  //create graph
  val fg = new FactorGraph(potentials,
    p => new FactorContent(messageCalculator(p)), //p.defaultMaxMarginalizer
    v => new NodeContent(Message.empty(v)),
    (p, v) => new EdgeContent(Message.empty(v), Message.empty(v))
  )

  /**
   * Prepare the incoming messages for a factor->node message calculation.
   * @param factor the factor for which we need incoming messages
   * @param without the variable to which we need to send the eventual factor->node message to.
   * @return the incoming messages to the factor, without a message for the 'without' variable.
   */
  def incoming(factor: fg.Factor, without: Variable[Any]) = new Messages {
    def message[V](variable: Variable[V]) =
      if (variable == without) Message.empty(variable)
      else
        factor.edges.find(_.node.variable == variable).get.content.n2f.asInstanceOf[Message[V]]
    def variables = factor.term.variables.toSet
  }

  /**
   * Method used for message normalization of factor->node messages.
   */
  def normalize(message: Message[Any]) = message.normalizeByMax

  /**
   * Method used for calculating residuals of factor->node messages.
   */
  def residue(newMessage: Message[Any], oldMessage: Message[Any]) = (newMessage - oldMessage).norm1

  /**
   * Calculates the message from a factor to a given neighbor node.
   * @param factor the factor from which the message will come
   * @param incoming the incoming messages to the factor
   * @param target the target node to send the message to
   * @return the message to the target node.
   */
  def calculateOutgoingFactorMessage(factor: fg.Factor, incoming: Messages, target: Variable[Any]): Message[Any]

  /**
   * Updates the current message from factor to node, and returns the message residual
   * (change between this and previous message).
   * @param edge the edge between factor and node.
   * @return the residual of the message (difference to previous message), that can be used to test convergence.
   */
  def updateFactor2Node(edge: fg.Edge): Double = {
    val variable = edge.node.variable
    val in = incoming(edge.factor, variable)
    val out = calculateOutgoingFactorMessage(edge.factor, in, variable)
    val old = edge.content.f2n
    edge.content.f2n = normalize(out).memoize
    residue(edge.content.f2n, old)
  }

  //updates message from node to factor
  /**
   * Updates the current message from node to factor.
   * @param penalties these are external penalties/messages that have been passed on to the inference algorithm
   *                  and need to be taken into account.
   * @param edge the factor-node edge to update the message on.
   */
  def updateNode2Factor(penalties: Messages, edge: fg.Edge) {
    var result = penalties.message(edge.node.variable)
    for (other <- edge.node.edges; if (other != edge))
      result = messageAggregation(result, translate(other, edge, other.content.f2n))
    edge.content.n2f = result
  }

  /**
   * During message passing incoming messages to a node need to be aggregated. This method defines how to.
   * @param msg1 the first message to aggregate
   * @param msg2 the second message to aggregate
   * @return the aggregated message.
   */
  def messageAggregation(msg1: Message[Any], msg2: Message[Any]) = msg1 + msg2

  /**
   * Translates message to node before message aggregation,
   * according to the factor that will receive the node to factor message.
   */
  def translate(other:fg.Edge, edge: fg.Edge, msg:Message[Any]):Message[Any] = msg

  /**
   * Performs message passing until convergence, and updates node beliefs afterwards.
   * @param penalties incoming messages on the variables.
   * @param vars set of variables to calculate beliefs for.
   */
  def messagePassing(penalties: Messages, vars: Iterable[Variable[Any]]) {

    //perform message passing (on edges)
    var i = 0
    var maxResidual = Double.PositiveInfinity
    while (i < 10 && maxResidual > 0.001) {
      maxResidual = 0.0
      for (edge <- fg.edges) {
        for (other <- edge.factor.edges; if (other != edge)) updateNode2Factor(penalties, other)
        maxResidual = math.max(updateFactor2Node(edge), maxResidual)
      }
      println(maxResidual)
      i += 1
    }

    //calculate node marginals
    for (n <- fg.nodes) n.content.belief = n.edges.view.map(_.content.f2n).foldLeft(penalties.message(n.variable))(_ + _)

  }

}


