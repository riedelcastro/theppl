package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import scala.io.Source
import scala.Array
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.infer.{GenericFactorGraph, GenericMessagePassing}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import breeze.linalg._
import com.github.riedelcastro.theppl.LabelVar
import com.github.riedelcastro.theppl.term.SeqTerm
import com.github.riedelcastro.theppl.StringVar
import com.github.riedelcastro.theppl.term.Dom

/**
 * @author Sebastian Riedel
 */
object KernelBPDemo {

  import TermImplicits._

  def main(args: Array[String]) {

    val n = 2
    val dom = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)

    case class Token(index: Int, word: String, tag: String, chunk: String)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val sentences = Source.fromInputStream(stream).getLines().take(100).foldLeft(Seq(Seq.empty[String])) {
      (result, line) => if (line == "") result :+ Seq.empty[String] else result.init :+ (result.last :+ line)
    }
    def toToken(line: String, index: Int) = {
      val Array(word, tag, chunk) = line.split("\\s+")
      Token(index, word, tag, chunk)
    }
    val tokenized = sentences.take(n).map(_.zipWithIndex.map({ case (line, index) => toToken(line, index) }))
    println(tokenized.mkString("\n"))

    //local variables within the chunk and pos values
    val currentChunk = LabelVar('chunk, dom)
    val currentTag = StringVar('tag)

    val transitionData = new ArrayBuffer[(State, State)]
    val observationData = new ArrayBuffer[(State, State)]
    val goldSequences = new ArrayBuffer[State]

    case class DataVar[Id](id: Id) extends Variable[EdgeData] {
      def domain = Util.infiniteSeq
    }

    //Variables that capture the data (and representations of the data used for quick inference)
    val t = DataVar('t)
    val o = DataVar('o)

    //all chains
    val chains = new ArrayBuffer[Term[Double]]

    //predicates
    //val chunkPred = 'chunk := Ints -> States
    //val tagPred = 'tag := Ints -> States


    //create variables and states
    for (sentence <- tokenized) {
      val chunkVars = for (token <- sentence) yield StateVariable('chunk -> token)
      val chunkValues = for (token <- sentence) yield State(Map(currentChunk -> token.chunk))
      val tagVars = for (token <- sentence) yield StateVariable('tag -> token)
      val tagValues = for (token <- sentence) yield State(Map(currentTag -> token.tag))

      val transitionEdges = chunkVars.dropRight(1) zip chunkVars.drop(1) map { case (c1, c2) => NonparametricEdge(c1, c2, t) }
      val observationEdges = tagVars zip chunkVars map { case (p, c) => NonparametricEdge(p, c, o) }

      val chain = DoubleAddN(SeqTerm(observationEdges ++ transitionEdges))

      chains += chain

      //create the data assignment
      val vars2Values = new mutable.HashMap[Variable[Any], Any]
      for (i <- 0 until sentence.size) {
        vars2Values(chunkVars(i)) = chunkValues(i)
        vars2Values(tagVars(i)) = tagValues(i)
      }
      goldSequences += State(vars2Values.toMap)

    }

    //collect training data from chains
    val trainN = 1
    val data = for ((chain, gold) <- chains.take(trainN) zip goldSequences.take(trainN);
                    NonparametricEdge(arg1, arg2, dataVar) <- Unroller.unrollDoubleSum(chain)) yield
      dataVar ->(gold(arg1), gold(arg2))

    val var2Data = data.groupBy(_._1)

    val $ = new Index()
    val chunkFeatures = $('c0, currentChunk) --> 1.0
    val chunkKernel = LinearKernel(chunkFeatures)
    val tagFeatures = $('t0, currentTag) --> 1.0
    val tagKernel = LinearKernel(tagFeatures)

    val transitionModel = NonparametricPreprocessor.preprocessData(var2Data(t).map(_._2), chunkKernel, chunkKernel)
    val observatioModel = NonparametricPreprocessor.preprocessData(var2Data(o).map(_._2), tagKernel, chunkKernel)

    val model = State(Map(t -> transitionModel, o -> observatioModel))

    def chunkDistribution(msg:KBPMessage) = for (chunkLabel <- dom) yield {
      val state = State(Map(currentChunk -> chunkLabel))
      val prob = msg.belief(state)
      chunkLabel -> prob
    }


    //now look at test chains
    for ((chain, gold) <- chains.drop(trainN) zip goldSequences.drop(trainN)) {
      val edges = Unroller.unrollDoubleSum(chain).collect({ case n: NonparametricEdge => n })
      val chunkVars = chain.variables.collect({ case v@StateVariable(('chunk, _)) => v }).toSet
      val tagVars = chain.variables.collect({ case v@StateVariable(('tag, _)) => v }).toSet
      val observation = gold.hide(chunkVars.map(_.asInstanceOf[Variable[Any]])) + model
      val result = KernelBP.messagePassing(edges, observation)
      val first = result(chunkVars.head)
      println(chunkVars.head)
      println(first)
      println(chunkDistribution(first).map{case (chunk,prob) => "%6.3f %s".format(prob, chunk)}.mkString("\n"))

    }

  }

}

//case class NonparametricModel(edges:Seq[NonparametricEdge]) extends FunApp1(DoubleAddN,SeqTerm(edges))

object KernelBP {

  class FactorContent
  class NodeContent(var belief: DenseVector[Double], var nodeData: NodeData)
  class EdgeContent(var f2n: DenseVector[Double], var n2f: DenseVector[Double])

  class FG(val terms: Seq[NonparametricEdge]) extends GenericFactorGraph {
    type PotentialType = NonparametricEdge
    type VariableType = Variable[State]
    type FactorContentType = FactorContent
    type EdgeContentType = EdgeContent
    type NodeContentType = NodeContent
    def createFactorContent(potential: PotentialType) = new FactorContent
    def createNodeContent(variable: VariableType) = new NodeContent(null, null)
    def createEdgeContent(potential: PotentialType, variable: VariableType) =
      new EdgeContent(null, null)
    def variables(potential: PotentialType) = Seq(potential.arg1, potential.arg2)
  }

  def messagePassing(potentials: Seq[NonparametricEdge], observation: State):Map[Variable[State],KBPMessage] = {
    val fg = new FG(potentials)
    val observedVariables = observation.variables
    val oneObserved = fg.factors.filter(f => observedVariables(f.term.arg1) || observedVariables(f.term.arg2))
    val bothHidden = fg.factors.filter(f => !observedVariables(f.term.arg1) && !observedVariables(f.term.arg2))

    //println(oneObserved.mkString("\n"))

    def normalizeMsg(msg:DenseVector[Double]) {
      val sum = msg.sum
      if (math.abs(sum) > 0.0)
        msg :*= 1.0 / sum
      else
        msg := 1.0 / msg.length
    }

    //calculate message from observed to unobserved variable
    for (factor <- oneObserved) {
      val dataRepr = observation(factor.term.data)
      val term = factor.term
      val (obsVar, kernel, dstEdge, data, transition) = if (observedVariables(term.arg1))
        (term.arg1, dataRepr.kernel1, factor.edges.find(_.node.variable == term.arg2).get, dataRepr.data.map(_._1), dataRepr.obs12)
      else
        (term.arg2, dataRepr.kernel2, factor.edges.find(_.node.variable == term.arg1).get, dataRepr.data.map(_._2), dataRepr.obs12)
      val obs = observation(obsVar)
      val orig = new DenseVector[Double](Array.ofDim[Double](dataRepr.m))
      for (i <- 0 until dataRepr.m) orig(i) = kernel(data(i), obs)
      val msg = transition * orig
      normalizeMsg(msg)
      dstEdge.content.f2n = msg
    }

    def isEdge1(edge: fg.Edge) =
      edge.node.variable == edge.factor.term.arg1
    def nodeDataFor(edge: fg.Edge) =
      if (isEdge1(edge)) observation(edge.factor.term.data).nodeData1 else observation(edge.factor.term.data).nodeData2

    val sameKernelTranslations = new mutable.HashMap[(NodeData, NodeData), DenseMatrix[Double]]

    def computePreMessage(edges: Seq[fg.Edge], targetData: NodeData): DenseVector[Double] = {
      val preMessage = new DenseVector[Double](Array.fill(targetData.data.size)(1.0))
      for (other <- edges) {
        val otherData = nodeDataFor(other)
        val translation = sameKernelTranslations.getOrElseUpdate(otherData -> targetData, {
          val result = new DenseMatrix[Double](targetData.data.size, otherData.data.size)
          for (col <- 0 until otherData.data.size; row <- 0 until targetData.data.size) {
            result(row, col) = targetData.kernel(otherData.data(col), targetData.data(row))
          }
          result
        })
        val fromOther = other.content.f2n
        require(fromOther != null)
        preMessage :*= translation * fromOther
      }
      preMessage

    }


    def passMessage(factor: fg.Factor, forward: Boolean) {
      val dataRepr = observation(factor.term.data)
      val term = factor.term
      val (in, out) = if (forward && factor.edges(0).node.variable == term.arg1 || !forward && factor.edges(1).node.variable == term.arg1)
        (factor.edges(0), factor.edges(1))
      else (factor.edges(1), factor.edges(0))
      val transition = if (forward) dataRepr.transition12 else dataRepr.transition21
      val inData = nodeDataFor(in)
      //pre-message calculation
      val preMessage = computePreMessage(in.node.edges.filter(_ != in), inData)
      val outMessage = transition * preMessage
      normalizeMsg(outMessage)
      out.content.f2n = outMessage
    }

    //forward pass
    for (factor <- bothHidden) {
      passMessage(factor, true)
    }

    //backward pass
    for (factor <- bothHidden.view.reverse) {
      passMessage(factor, false)
    }

    //calculate beliefs at each node
    val result = for (node <- fg.nodes; if !observedVariables(node.variable)) yield {
      node.content.nodeData = nodeDataFor(node.edges.head)
      node.content.belief = computePreMessage(node.edges, node.content.nodeData)
      normalizeMsg(node.content.belief)
      node.variable -> KBPMessage(node.content.belief,node.content.nodeData)
    }
    result.toMap


  }

}

case class NonparametricEdge(arg1: Variable[State], arg2: Variable[State],
                             data: Variable[EdgeData]) extends Potential {
  def hidden = Seq(arg1, arg2, data)
  def score(state: State) = {
    //todo:
    sys.error("Don't know how to calculate the explicit potential score for nonparametric edge")
  }
}


case class NodeData(kernel: Kernel, data: Seq[State]) {
  def m = data.size
}

case class EdgeData(nodeData1: NodeData, nodeData2: NodeData,
                    transition12: DenseMatrix[Double], transition21: DenseMatrix[Double],
                    obs12: DenseMatrix[Double], obs21: DenseMatrix[Double]) {
  def m = data.size
  def data = nodeData1.data zip nodeData2.data
  def kernel1 = nodeData1.kernel
  def kernel2 = nodeData2.kernel
}

case class KBPMessage(weighting: DenseVector[Double], data: NodeData) {
  def belief(state: State) = {
    var result = 0.0
    for (index <- data.data.indices; weight = weighting(index); if math.abs(weight) > 0.0) {
      result += weight * data.kernel(data.data(index), state)
    }
    //result /= data.m
    result
  }
  override def toString = {
    val lines = for (index <- data.data.indices; weight = weighting(index);
                     if math.abs(weight) > 0.0; state = data.data(index)) yield
      "%6.3f %s".format(weight, state)
    lines.mkString("\n")
  }
}


object NonparametricPreprocessor {

  case class Result(transition1: Matrix[Double], transition2: Matrix[Double])

  def preprocessData(data: Seq[(State, State)], kernel1: Kernel, kernel2: Kernel, lambda: Double = 0.1) = {
    //calculate first gram matrix
    val G1 = new DenseMatrix[Double](data.size, data.size)
    val G2 = new DenseMatrix[Double](data.size, data.size)
    for (i <- 0 until data.size) {
      val (x1, x2) = data(i)
      for (j <- i until data.size) {
        val (y1, y2) = data(j)
        val sim1 = kernel1(x1, y1)
        val sim2 = kernel2(x2, y2)
        G1(i, j) = sim1
        G1(j, i) = sim1
        G2(i, j) = sim2
        G2(j, i) = sim2
      }
    }
    //do regularization + inversion
    val R = DenseMatrix.eye[Double](data.size) * lambda * data.size.toDouble
    val G1_R = G1 + R
    val G2_R = G2 + R
    val T1 = LinearAlgebra.inv(G1_R)
    val T2 = LinearAlgebra.inv(G2_R)
    val Obs12 = LinearAlgebra.inv(G1_R * G2_R)
    val Obs21 = LinearAlgebra.inv(G2_R * G1_R)
    val nodeData1 = NodeData(kernel1, data.map(_._1))
    val nodeData2 = NodeData(kernel2, data.map(_._2))

    EdgeData(nodeData1, nodeData2, T1, T2, Obs12, Obs21)
  }

}


case class StateVariable[+Id](id: Id) extends Variable[State] {
  def domain = States.values
}

object States extends Dom[State]('states, Util.infiniteSeq)

trait Kernel {
  def apply(x: State, y: State): Double
}

case class LinearKernel(feature: Term[Vec]) extends Kernel {
  def apply(x: State, y: State) = {
    val fx = feature.eval(x).get
    val fy = feature.eval(y).get
    fx dot fy
  }
}