package com.github.riedelcastro.theppl.examples

import com.github.riedelcastro.theppl.util.Util
import io.Source
import com.github.riedelcastro.theppl._

/**
 * @author sriedel
 */
object ClassifierExample {

  def main(args: Array[String]) {

    val chunks = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)
    case class Token(index: Int, word: String, tag: String, chunk: String) extends Var(chunks)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val indexedLines = Source.fromInputStream(stream).getLines.take(10).filter(_ != "").zipWithIndex
    val tokens = for ((line, index) <- indexedLines.toSeq; Array(word, tag, chunk) = line.split("\\s+")) yield
      Token(index, word, tag, chunk)
    val lifted = tokens.lift
    val training = for (token <- tokens) yield new TrainingInstance(token, new SingletonState(token, token.chunk))
    val contextFeats = (token: Token) =>
      new ParameterVector(Seq(Feat("t", token.tag), Feat("t-1", lifted(token.index - 1).map(_.tag))))
    def labelFeats = (label: String) =>
      new ParameterVector(Seq(Feat(label)) ++ label.split("-").map(Feat("split", _)))
    val classifier = new Classifier(contextFeats, labelFeats) with OnlineLearner
    classifier.train(training)
    println(tokens.mkString("\n"))
    println(classifier.weights)

  }

}