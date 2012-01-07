package com.github.riedelcastro.theppl.examples

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import io.Source
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
    case class ChunkVar(token: Token) extends Variable[String]

    val domain = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)

    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val indexedLines = Source.fromInputStream(stream).getLines().take(n).filter(_ != "").zipWithIndex
    val tokens = for ((line, index) <- indexedLines.toSeq; Array(word, tag, chunk) = line.split("\\s+")) yield
      Token(index, word, tag, chunk)

    val lifted = tokens.lift
    def labelFeatures(label: String) = fromFeats(Seq(Feat(label)) ++ label.split("-").map(Feat("split", _)))
    def tokenFeatures(token: Token) = fromPairs("t" -> token.tag, "t-1" -> lifted(token.index - 1).map(_.tag))

    val classifier = Classifier[String, Token, ChunkVar](ChunkVar(_), domain, tokenFeatures(_), labelFeatures(_))
    val learner = new classifier.decorated with OnlineLearner with PerceptronUpdate

    val instances = for (token <- tokens) yield new Instance(token, ChunkVar(token) -> token.chunk)
    val train = instances.take(instances.size / 2)
    val test = instances.drop(instances.size / 2)
    println(Evaluator.evaluate(classifier, train))

    learner.train(train)
    println(Evaluator.evaluate(classifier, train))
    println(Evaluator.evaluate(classifier, test))

    println(classifier.weights)

    val out = new ByteArrayOutputStream(1000)
    classifier.save(out)

    val in = new ByteArrayInputStream(out.toByteArray)
    val copy = Classifier[String, Token, ChunkVar](ChunkVar(_), domain, tokenFeatures(_), labelFeatures(_))
//    copy.load(in)
//    println(Evaluator.evaluate(copy, test))


    //    val decorated = new classifier.Wrap with OnlineLearner with PerceptronUpdate

  }

}

object Evaluator {

  def evaluate[C](module: Module {type Context = C}, instances: Seq[Instance[C]]) = {
    var totalLoss = 0.0
    var count = 0
    for (instance <- instances) {
      val gold = instance.gold
      val model = module.model(instance.context, instance.observation)
      val guess = model.predict
      for (hidden <- model.hidden) {
        if (gold(hidden) != guess(hidden)) totalLoss += 1.0
        count += 1
      }
    }
    totalLoss / count
  }

}