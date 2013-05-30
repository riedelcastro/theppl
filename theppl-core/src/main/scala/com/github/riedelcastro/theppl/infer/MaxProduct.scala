package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.term.{TermImplicits, Term, Unroller, Loglinear}
import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.CollectionUtil
import scala.util.Random
import com.github.riedelcastro.theppl.LabelVar
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
class MaxProduct(val potential:Potential) extends MaxMarginalizer {


  class FactorContent(val maxMarginalizer: MaxMarginalizer)
  class EdgeContent(var f2n: Message[Any], var n2f: Message[Any])
  class NodeContent(var belief: Message[Any])


  def maxMarginals(penalties: Messages, vars: Iterable[Variable[Any]]) = {

    val unrolled = Unroller.unrollDoubleSum(potential).map(TermImplicits.toPotential)

    //create graph
    val fg = new FactorGraph(unrolled,
      p => new FactorContent(MaxMarginalizers.exhaustive(p)),
      v => new NodeContent(Message.empty(v)),
      (p, v) => new EdgeContent(Message.empty(v), Message.empty(v))
    )

    //prepares the incoming message to a factor
    def incoming(factor: fg.Factor, without: Variable[Any]) = new Messages {
      def message[V](variable: Variable[V]) =
        if (variable == without) Message.empty(variable)
        else
          factor.edges.find(_.node.variable == variable).get.content.n2f.asInstanceOf[Message[V]]
      def variables = factor.term.variables.toSet
    }

    //update message from factor to node
    def updateFactor2Node(edge: fg.Edge): Double = {
      val variable = edge.node.variable
      val in = incoming(edge.factor, variable)
      val out = edge.factor.content.maxMarginalizer.maxMarginals(in, Seq(variable))
      val old = edge.content.f2n
      edge.content.f2n = out.messages.message(variable)
      (edge.content.f2n - old).norm1
    }

    //updates message from node to factor
    def updateNode2Factor(edge: fg.Edge) {
      var result = penalties.message(edge.node.variable)
      for (other <- edge.node.edges; if (other != edge)) result = result + other.content.f2n
      edge.content.n2f = result.normalize.memoize
    }

    //perform message passing (on edges)
    var i = 0
    var maxResidual = Double.PositiveInfinity
    while (i < 10 && maxResidual > 0.001) {
      maxResidual = 0.0
      for (edge <- fg.edges) {
        for (other <- edge.factor.edges; if (other != edge)) updateNode2Factor(other)
        maxResidual = math.max(updateFactor2Node(edge), maxResidual)
      }
      println(maxResidual)
      i += 1
    }

    //calculate node marginals
    for (n <- fg.nodes) n.content.belief = n.edges.view.map(_.content.f2n).foldLeft(penalties.message(n.variable))(_ + _)

    MaxMarginalizationResult(
      messages = new Messages {
        def message[V](variable: Variable[V]) = fg.variable2Node(variable).content.belief.asInstanceOf[Message[V]]
        def variables = vars.toSet
      },
      max = 0.0 // todo
    )
  }


}

object MaxProduct {
  import com.github.riedelcastro.theppl.term.TermImplicits._

  def main(args: Array[String]) {

    val random = new Random(10)
    def table(arg1: Variable[Any], arg2: Variable[Any]) = Table(Seq(arg1, arg2), { case _ => random.nextGaussian() })
    val Domain = Seq(1, 2, 3)
    val Seq(a, b, c) = Seq('A, 'B, 'C).map(LabelVar(_, Domain))
    val terms = Seq(a -> b, b -> c, c -> a).map(p => table(p._1, p._2))
    val Seq(ab, bc, ca) = terms
    val model = ab + bc + ca

    val maxProduct = new MaxProduct(model)

    val maxMarginals = maxProduct.maxMarginals()
    val exact = model.argmax()

    println(maxMarginals.messages)
    println(maxMarginals.messages.argmaxState)
    println(exact.state)





  }

}
