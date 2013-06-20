package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import scala.io.Source
import scala.Array
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.infer.GenericMessagePassing
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import breeze.linalg._
import com.github.riedelcastro.theppl.LabelVar
import com.github.riedelcastro.theppl.StringVar
import com.github.riedelcastro.theppl.term.Dom

/**
 * @author Sebastian Riedel
 */
object KernelBPDemo {

  import TermImplicits._

  def main(args: Array[String]) {

    val n = 1
    val dom = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)

    case class Token(index: Int, word: String, tag: String, chunk: String)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val sentences = Source.fromInputStream(stream).getLines().take(100).foldLeft(Seq(Seq.empty[String])) {
      (result, line) => if (line == "") result :+ Seq.empty[String] else result.init :+ (result.last :+ line)
    }
    def toToken(line: String) = {
      val Array(word, tag, chunk) = line.split("\\s+")
      Token(0, word, tag, chunk)
    }
    val tokenized = sentences.take(n).map(_.map(toToken))
    println(tokenized.mkString("\n"))

    //local variables within the chunk and pos values
    val currentChunk = LabelVar('chunk, dom)
    val currentTag = StringVar('tag)

    val transitionData = new ArrayBuffer[(State, State)]
    val observationData = new ArrayBuffer[(State, State)]
    val goldSequences = new ArrayBuffer[State]

    //create variables and states
    for (sentence <- tokenized) {
      val chunkVars = for (token <- sentence) yield StateVariable('chunk -> token)
      val chunkValues = for (token <- sentence) yield State(Map(currentChunk -> token.chunk))
      val tagVars = for (token <- sentence) yield StateVariable('tag -> token)
      val tagValues = for (token <- sentence) yield State(Map(currentTag -> token.tag))

      transitionData ++= chunkValues.dropRight(1) zip chunkValues.drop(1)
      observationData ++= tagValues zip chunkValues

      //create the data assignment
      val vars2Values = new mutable.HashMap[Variable[Any], Any]
      for (i <- 0 until sentence.size) {
        vars2Values(chunkVars(i)) = chunkValues(i)
        vars2Values(tagVars(i)) = tagValues(i)
      }
      goldSequences += State(vars2Values.toMap)
    }

    val $ = new Index()
    val chunkFeatures = $('c0,currentChunk) --> 1.0
    val chunkKernel = LinearKernel(chunkFeatures)
    val tagFeatures = $('t0,currentTag) --> 1.0
    val tagKernel = LinearKernel(tagFeatures)

    val transitionModel = EdgeLearner.learn(transitionData,chunkKernel,chunkKernel)
    println(transitionModel.transition1)

  }

}

object EdgeLearner {

  case class Result(transition1: Matrix[Double], transition2: Matrix[Double])

  def learn(data: Seq[(State, State)], kernel1: Kernel, kernel2: Kernel, lambda:Double = 0.1) = {
    //calculate first gram matrix
    val G1 = new DenseMatrix[Double](data.size, data.size)
    val G2 = new DenseMatrix[Double](data.size, data.size)
    for (i <- 0 until data.size) {
      val (x1, x2) = data(i)
      for (j <- i until data.size) {
        val (y1,y2) = data(j)
        val sim1 = kernel1(x1, y1)
        val sim2 = kernel2(x2, y2)
        G1(i,j) = sim1
        G1(j,i) = sim1
        G2(i,j) = sim2
        G2(j,i) = sim2
      }
    }
    //do regularization + inversion
    val R = DenseMatrix.eye[Double](data.size) * lambda * data.size.toDouble
    val T1 = LinearAlgebra.inv(G1 + R)
    val T2 = LinearAlgebra.inv(G2 + R)
    Result(T1,T2)
  }

}

class KernelBP extends GenericMessagePassing {
  type MessageCalculator = this.type
  def messageCalculator(potential: Potential) = ???
  def potentials = ???
  def calculateOutgoingFactorMessage(factor: fg.Factor, incoming: Messages, target: Variable[Any]) = ???
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