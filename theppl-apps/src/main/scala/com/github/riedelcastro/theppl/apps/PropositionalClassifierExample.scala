package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import io.Source
import learn._
import term.{Loglinear, Index}

/**
 * @author sriedel
 */
object PropositionalClassifierExample {

  import com.github.riedelcastro.theppl.term.TermImplicits._

  def main(args: Array[String]) {

    val n = 100
    case class Token(index: Int, word: String, tag: String, chunk: String)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val indexedLines = Source.fromInputStream(stream).getLines().take(n).filter(_ != "").zipWithIndex
    val tokens = for ((line, index) <- indexedLines.toSeq; Array(word, tag, chunk) = line.split("\\s+")) yield
      Token(index, word, tag, chunk)
    val lifted = tokens.lift

    val dom = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)
    val chunk = LabelVar('chunk, dom)
    val word = StringVar('word)
    val tag = StringVar('tag)
    val tag_m1 = StringVar('tag_m1)
    val $ = new Index()

    def toState(index: Int, token: Token, tokens: Int => Option[Token]) = State(Map(
      Target(chunk) -> token.chunk,
      word -> token.word,
      tag -> token.tag,
      tag_m1 -> tokens(index - 1).map(_.tag).getOrElse("NA")))


    val weights = VecVar('weights)
    val feats = { $('t, tag, chunk) --> 1.0 } + { $('w, word, chunk) --> 1.0 } + { $('tm1, tag_m1, chunk) --> 1.0 }
    val model = Loglinear(feats, weights)

    val trainTokens = Range(0, tokens.size / 2).map(i => toState(i, tokens(i), lifted))
    val testTokens = Range(tokens.size / 2, tokens.size).map(i => toState(i, tokens(i), lifted))

    val learnedWeights = LinearLearner.learn(model)(trainTokens)
    val trained = model | weights -> learnedWeights

    println(learnedWeights)
    println(testTokens.size)
    val evals = Evaluator.evaluate(testTokens.map(_.target), testTokens.map(s => (trained | s).argmax().state), Seq(chunk))
    println(evals.mkString("\n"))


  }

  case class Evaluation(variable: Variable[Any], tp: Int, tn: Int, fp: Int, fn: Int) {
    def totalGold = tp + fn
    def totalGuess = tp + fp
    def precision = tp.toDouble / totalGuess
    def recall = tp.toDouble / totalGold
    def +(that: Evaluation) = Evaluation(variable, tp + that.tp, tn + that.tn, fp + that.fp, fn + that.fn)
    override def toString =
      """
        |Variable:    %s
        |Total Gold:  %d
        |Total Guess: %d
        |Precision:   %f
        |Recall:      %f
      """.stripMargin.format(variable,totalGold,totalGuess,precision,recall)
  }

  object Evaluator {
    def evaluate(gold: Seq[State], guess: Seq[State], variables: Seq[Variable[Any]]) = {
      val results = for ((target, prediction) <- gold.view.zip(guess.view)) yield {
        for (v <- variables) yield {
          val t = target(v)
          val p = prediction(v)
          val tp = if (t != v.default && t == p) 1 else 0
          val tn = if (t == v.default && t == p) 1 else 0
          val fp = if (p != v.default && t != p) 1 else 0
          val fn = if (p == v.default && t != p) 1 else 0
          Evaluation(v, tp, tn, fp, fn)
        }
      }
      val summed = results.reduce((vector1, vector2) => vector1.zip(vector2).map({ case (e1, e2) => e1 + e2 }))
      summed
    }
  }

}


