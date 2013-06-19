package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl.{Potential, State, Variable}
import com.github.riedelcastro.theppl.util.Util
import scala.io.Source
import scala.Array
import com.github.riedelcastro.theppl.term.{Vec, Term}

/**
 * @author Sebastian Riedel
 */
object KernelBP {

  def main(args: Array[String]) {

    val n = 1
    case class Token(index: Int, word: String, tag: String, chunk: String)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val sentences = Source.fromInputStream(stream).getLines().foldLeft(Seq(Seq.empty[String])) {
      (result, line) => if (line == "") result :+ Seq.empty[String] else result.init :+ (result.last :+ line)
    }
    def toToken(line: String) = {
      val Array(word, tag, chunk) = line.split("\\s+")
      Token(0, word, tag, chunk)
    }
    val tokenized = sentences.take(n).map(_.map(toToken))
    println(tokenized.mkString("\n"))


  }

}


class KernelBPPipelineModel(edges: Seq[Variable[State]],
                            kernels: Map[Variable[State], Kernel],
                            data: Seq[Seq[State]]) extends Potential {
  def hidden = kernels.keySet
  def score(state: State) = ???

  def prepareOperators() {

  }

  def infer(observation: State) = {
    //get
    null
  }
}

case class StateVariable[Id](id: Id) extends Variable[State] {
  def domain = Util.infinity
}

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