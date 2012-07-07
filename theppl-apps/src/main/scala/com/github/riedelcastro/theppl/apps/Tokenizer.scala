package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import infer.Expectator
import learn.{MaxentLearner2, SuperviseByDeterministicExpectations, MaxentLearner}
import ParameterVector._

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

object Tokenizer {
  def main(args: Array[String]) {
    case class Doc(src: String, goldSplits:Option[Set[Int]])
    case class SplitVar(doc: Doc, index: Int) extends BoolVariable

    val tokenizer = new Tokenizer[Doc] {
      def source(doc: Doc) = doc.src
      def splitVar(splitCandidate: SplitCandidate) = SplitVar(splitCandidate.doc, splitCandidate.offset)
      def truthAt(splitCandidate: SplitCandidate) = splitCandidate.doc.goldSplits.map(_.apply(splitCandidate.offset))

      def candidates(doc: Doc) = for (index <- 0 until doc.src.length; if (doc.src(index) == '.')) yield
        SplitCandidate(doc, index)
    }

    val learner = new MaxentLearner2[Doc] {
      val template = tokenizer
      def instances = Seq.empty
    }
//
//    learner.train()

    //learner.learn(tokenizer, docs)
    //tokenizer.learn((doc,potential) -> doc.splits.map(i -> SplitVar(i)))
    //tokenizer.argmax(doc) =

  }
}