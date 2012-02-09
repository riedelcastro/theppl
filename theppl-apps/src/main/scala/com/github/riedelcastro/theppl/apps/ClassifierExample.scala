package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import infer.Argmaxer
import io.Source
import learn._
import ParameterVector._
import Imports._
import java.io.ByteArrayOutputStream

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

    val classifier = new Classifier[Token]  {
      type LabelType = String
      type LabelVariableType = ChunkVar
      def variable(context: Token) = ChunkVar(context)
      def labelFeatures(label: LabelType) = fromFeats(Seq(Feat(label)) ++ label.split("-").map(Feat("split", _)))
      def contextFeatures(token: Token) = fromPairs("t" -> token.tag, "t-1" -> lifted(token.index - 1).map(_.tag))
    }


    val trainTokens = tokens.take(tokens.size / 2)
    val testTokens = tokens.drop(tokens.size / 2)

    val evaluator = new Evaluator[Token] {
      val module = classifier
      def targetState(context: Token, model: module.ModelType) = model.labelVariable -> context.chunk
      def argmaxer(model: ModelType) = Argmaxer(model)
    }

    println(evaluator.evaluate(trainTokens))

    val learner = new OnlineLearner[Token] with PerceptronUpdate {
      val module = classifier
      def targetState(context: Token, model: module.ModelType) = model.labelVariable -> context.chunk
      def argmaxer(model: module.ModelType) = Argmaxer(model)
      def instances = trainTokens
    }

    learner.train()
    println(evaluator.evaluate(trainTokens))
    println(evaluator.evaluate(testTokens))

    println(classifier.weights)

    val out = new ByteArrayOutputStream(1000)
    classifier.save(out)

    //    copy.load(in)
    //    println(Evaluator.evaluate(copy, test))


    //    val decorated = new classifier.Wrap with OnlineLearner with PerceptronUpdate

  }

}


