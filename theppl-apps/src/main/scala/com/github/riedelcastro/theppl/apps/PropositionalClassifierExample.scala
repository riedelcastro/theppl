package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util.Util
import infer.Argmaxer
import io.Source
import learn._
import ParameterVector._
import Imports._
import java.io.ByteArrayOutputStream
import term.{LearnerNew, Loglinear, Index}

/**
 * @author sriedel
 */
object PropositionalClassifierExample {

  import com.github.riedelcastro.theppl.term.TermImplicits._

  def main(args: Array[String]) {

    val n = 50
    case class Token(index: Int, word: String, tag: String, chunk: String)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val indexedLines = Source.fromInputStream(stream).getLines().take(n).filter(_ != "").zipWithIndex
    val tokens = for ((line, index) <- indexedLines.toSeq; Array(word, tag, chunk) = line.split("\\s+")) yield
      Token(index, word, tag, chunk).asInstanceOf[Token] //todo: IDE problem???
    val lifted = tokens.lift

    val dom = Seq("O") ++ (for (bi <- Seq("B-", "I-"); t <- Seq("VP", "NP", "PP")) yield bi + t)
    val chunk = LabelVar('chunk,dom)
    val word = StringVar('word)
    val tag = StringVar('tag)
    val $ = new Index()

    def toState(token:Token) = State(Map(Target(chunk) -> token.chunk, word -> token.word, tag -> token.tag))

    val weights = VecVar('weights)
    val feats = {$('t,tag,chunk) --> 1.0 } + {$('w,word,chunk) --> 1.0}
    val model = Loglinear(feats,weights)

    val trainTokens = tokens.take(tokens.size / 2).map(toState)
    val testTokens = tokens.drop(tokens.size / 2).map(toState)

    val trained = LearnerNew.learn(model)(trainTokens)
    val conditioned = model | weights -> trained



//
//    case class ChunkVar(token: Token) extends Variable[String] {
//      def domain = dom
//    }
//
//
//    val classifier = new Classifier[Token]  {
//      type LabelType = String
//      type LabelVariableType = ChunkVar
//      def variable(context: Token) = ChunkVar(context)
//      def labelFeatures(label: LabelType) = fromFeats(Seq(Feat(label)) ++ label.split("-").map(Feat("split", _)))
//      def contextFeatures(token: Token) = fromPairs("t" -> token.tag, "t-1" -> lifted(token.index - 1).map(_.tag))
//      override def truth(context: Token) = Some(context.chunk)
//    }
//
//
//    val trainTokens = tokens.take(tokens.size / 2)
//    val testTokens = tokens.drop(tokens.size / 2)
//
//    val evaluator = new Evaluator[Token] {
//      val template = classifier
//      def targetState(context: Token, potential: template.PotentialType) = potential.labelVariable -> context.chunk
//      def argmaxer(potential: PotentialType) = Argmaxer(potential)
//    }
//
//    println(evaluator.evaluate(trainTokens))
//
//    val learner = new OnlineLearner[Token] with PerceptronUpdate {
//      val template = classifier
//      def instances = trainTokens
//    }
//
//    learner.train()
//    println(evaluator.evaluate(trainTokens))
//    println(evaluator.evaluate(testTokens))
//
//    println(classifier.weights)
//
//    val out = new ByteArrayOutputStream(1000)
//    classifier.save(out)

    //    copy.load(in)
    //    println(Evaluator.evaluate(copy, test))


    //    val decorated = new classifier.Wrap with OnlineLearnerOld with PerceptronUpdate

  }

}


