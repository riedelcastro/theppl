package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import infer.Expectator
import learn._
import ParameterVector._
import java.io.{FileInputStream, InputStream}
import java.util.Scanner
import util.Util

/**
 * @author sriedel
 */
trait Tokenizer[Doc] extends FeatureSumTemplate[Doc] {

  type FeatureArgumentContext = SplitCandidate
  type OtherArgumentContext = Nothing
  type PotentialType = TokenizerPotential

  case class SplitCandidate(doc: Doc, offset: Int)

  def source(doc: Doc): String
  def splitVar(splitCandidate: SplitCandidate): Variable[Boolean]
  def truthAt(splitCandidate: SplitCandidate): Option[Boolean]
  def candidates(doc: Doc): Seq[SplitCandidate]

  def otherArgs(context: Doc) = Seq.empty
  def featureArgs(context: Doc) = candidates(context)
  def featureTemplate = classifier
  def otherTemplate = EmptyTemplate

  def potential(c: Doc) = new TokenizerPotential {
    def context = c
  }

  lazy val classifier = new Classifier[SplitCandidate] {

    type LabelType = Boolean
    type LabelVariableType = Variable[Boolean]
    def variable(context: SplitCandidate) = splitVar(context)
    def labelFeatures(label: LabelType) = fromAny(Seq(label))
    def contextFeatures(context: SplitCandidate) = {
      val text = source(context.doc)
      val current = text(context.offset)
      fromPairs(
        "c" -> current,
        "c-u" -> current.isUpper
      )
    }
  }

  trait TokenizerPotential extends TemplatedFeatureSumPotential {
    override lazy val truth = State(featureContexts.flatMap({case c => truthAt(c).map(splitVar(c) -> _)}).toMap)
  }


}

object TokenizerMain {

  case class Doc(src: String, goldSplits: Option[Set[Int]])

  /**
   * Takes genia pos text file input and returns an iterator over Doc objects which can be used to train the tokenizer.
   * @param geniaTxt input stream in
   * @return
   */
  def loadGeniaDocs(geniaTxt: InputStream) = {
    val sentences = Util.streamIterator(geniaTxt, "====================\n")
    for (sentence <- sentences) yield {
      val tokens = sentence.split("\n")
      var txt = new StringBuffer()
      var charPos = 0
      var goldSplits: Set[Int] = Set.empty
      for (token <- tokens) {
        val word = token.substring(0,token.lastIndexOf('/'))
        if (word == "." || word == "," || word == ";") {
          goldSplits = goldSplits + charPos
          charPos += word.length
        } else if (charPos > 0) {
          txt.append(" ")
          charPos += 1
        }
        txt.append(word)
        charPos += word.length
      }
      Doc(txt.toString,Some(goldSplits))
    }

  }

  def main(args: Array[String]) {

    val inputFile = "/Users/riedelcastro/corpora/genia/GENIAcorpus3.02.pos.txt"
    val docs = loadGeniaDocs(new FileInputStream(inputFile)).take(10).toSeq

    println(docs.mkString("\n"))

    //theppl encourages clients to define/use their own variables, and configure the used modules/templates to use these
    //variables.
    case class SplitVar(doc: Doc, index: Int) extends BoolVariable

    val tokenizer = new Tokenizer[Doc] {
      def source(doc: Doc) = doc.src
      def splitVar(splitCandidate: SplitCandidate) = SplitVar(splitCandidate.doc, splitCandidate.offset)
      def truthAt(splitCandidate: SplitCandidate) = splitCandidate.doc.goldSplits.map(_.apply(splitCandidate.offset))

      def candidates(doc: Doc) = for (index <- 0 until doc.src.length; if (doc.src(index) == '.')) yield
        SplitCandidate(doc, index)
    }

    val learner = new OnlineLearner[Doc] with PerceptronUpdate {
      val template = tokenizer
      def instances = docs
    }

    learner.train()

  }
}