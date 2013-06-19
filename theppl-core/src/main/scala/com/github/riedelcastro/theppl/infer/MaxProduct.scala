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
class MaxProduct(val potential:Potential) extends GenericMessagePassing with MaxMarginalizer {


  type MessageCalculator = MaxMarginalizer
  type ResultType = MaxMarginalizationResult

  lazy val potentials = Unroller.unrollDoubleSum(potential).map(TermImplicits.toPotential)

  def messageCalculator(potential: Potential) = MaxMarginalizers.exhaustive(potential)

  def calculateOutgoingFactorMessage(factor: fg.Factor, incoming: Messages, target: Variable[Any]) = {
    factor.content.maxMarginalizer.maxMarginals(incoming, Seq(target)).messages.message(target)
  }

  def maxMarginals(penalties: Messages, variables: Iterable[Variable[Any]]) = {
    messagePassing(penalties,variables)
    createResult(variables)
  }

  def createResult(vars: Iterable[Variable[Any]]): ResultType = {
    val messages = new Messages {
      def message[V](variable: Variable[V]) = fg.variable2Node(variable).content.belief.asInstanceOf[Message[V]]
      def variables = vars.toSet
    }
    MaxMarginalizationResult(
      messages = messages,
      max = potential.score(messages.argmaxState)
    )
  }

}

object MaxProduct {
  import com.github.riedelcastro.theppl.term.TermImplicits._

  def main(args: Array[String]) {

    //this creates a table with random scores
    val random = new Random(20)
    def table(arg1: Variable[Any], arg2: Variable[Any]) = Table(Seq(arg1, arg2), { case _ => random.nextGaussian() })

    val Domain = Seq(1, 2, 3)  // Range(0,1000)
    val Seq(a, b, c) = Seq('A, 'B, 'C).map(LabelVar(_, Domain))
    val terms = Seq(a -> b, b -> c, c -> a).map(p => table(p._1, p._2))
    val Seq(ab, bc, ca) = terms
    val model = ab + bc + ca

    val maxProduct = new MaxProduct(model)

    val maxMarginals = maxProduct.maxMarginals()
    val exact = model.argmax()

    println(maxMarginals.messages)
    println("----")
    println(MaxMarginalizers.exhaustive(model).maxMarginals().messages)
    println("====")
    println(maxMarginals.messages.argmaxState)
    println(exact.state)





  }

}
