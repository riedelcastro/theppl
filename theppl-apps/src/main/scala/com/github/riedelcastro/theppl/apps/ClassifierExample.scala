package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import io.Source
import learn.{Learner, Instance, PerceptronUpdate, OnlineLearner}
import ParameterVector._
import Imports._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

/**
 * @author sriedel
 */
object ClassifierExample {

  def main(args: Array[String]) {

    val n = 50
    case class Token(index: Int, word: String, tag: String, chunk: String)
    val dom = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)
    case class ChunkVar(token: Token) extends Variable[String] {
      def domain = dom
    }

    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val indexedLines = Source.fromInputStream(stream).getLines().take(n).filter(_ != "").zipWithIndex
    val tokens = for ((line, index) <- indexedLines.toSeq; Array(word, tag, chunk) = line.split("\\s+")) yield
      Token(index, word, tag, chunk)
    val lifted = tokens.lift

    val classifier = new Classifier with OnlineLearner with PerceptronUpdate with Evaluator {
      type Context = Token
      type LabelType = String
      type LabelVariableType = ChunkVar
      def variable(context: Context) = ChunkVar(context)
      val domain = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)
      def labelFeatures(label: LabelType) = fromFeats(Seq(Feat(label)) ++ label.split("-").map(Feat("split", _)))
      def contextFeatures(token: Context) = fromPairs("t" -> token.tag, "t-1" -> lifted(token.index - 1).map(_.tag))
      def target(model: ModelType) = model.labelVariable -> model.labelVariable.token.chunk
    }

    val train = tokens.take(tokens.size / 2)
    val test = tokens.drop(tokens.size / 2)
    println(classifier.evaluate(train))

    classifier.train(train)
    println(classifier.evaluate(train))
    println(classifier.evaluate(test))

    println(classifier.weights)

    val out = new ByteArrayOutputStream(1000)
    classifier.save(out)

    //    copy.load(in)
    //    println(Evaluator.evaluate(copy, test))


    //    val decorated = new classifier.Wrap with OnlineLearner with PerceptronUpdate

  }

}

trait Evaluator extends Learner {
  def evaluate[C](instances: Seq[Context]) = {
    var totalLoss = 0.0
    var count = 0
    for (instance <- instances) {
      val model = this.model(instance)
      val gold = target(model)
      val guess = model.predict
      for (hidden <- model.hidden) {
        if (gold(hidden) != guess(hidden)) totalLoss += 1.0
        count += 1
      }
    }
    totalLoss / count
  }

}

