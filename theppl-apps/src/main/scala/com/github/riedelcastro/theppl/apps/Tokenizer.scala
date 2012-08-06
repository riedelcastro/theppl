package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.learn._
import infer.{NaiveFactoredArgmaxerRecipe, ArgmaxRecipe}
import nlp.{Token, Document}
import ParameterVector._
import java.io.{PrintStream, FileInputStream, InputStream}
import com.github.riedelcastro.theppl.util.Util
import scala.Some
import collection.mutable.ArrayBuffer

/**
 * This tokenizer predicts characters that serve as punctuation tokens. It is designed to
 * work with different underlying document representations. Please notice that the tokenizer
 * itself is a template too, and generates a document-wide potential that touches all
 * punctuation variables.
 *
 * @author sriedel
 */
trait Tokenizer[Doc] extends FeatureSumTemplate[Doc] {

  type FeatureArgumentContext = SplitCandidate
  type OtherArgumentContext = Nothing
  type PotentialType = TokenizerPotential

  case class SplitCandidate(doc: Doc, offset: Int)

  case class Token(doc: Doc, beginChar: Int, endChar: Int) {
    def word = source(doc).substring(beginChar, endChar)
  }

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

  private def digit = "\\d+".r

  lazy val classifier = new Classifier[SplitCandidate] {

    type LabelType = Boolean
    type LabelVariableType = Variable[Boolean]
    def variable(context: SplitCandidate) = splitVar(context)
    def labelFeatures(label: LabelType) = fromAny(Seq(label))
    def contextFeatures(context: SplitCandidate) = {
      val text = source(context.doc)
      val currentChar = text(context.offset)
      val prevWordBuffer = new StringBuilder
      var backwards = context.offset - 1
      while (backwards >= 0 && text(backwards) != ' ') {
        prevWordBuffer.insert(0, text(backwards))
        backwards -= 1
      }
      val prevWord = prevWordBuffer.toString()
      fromPairs(
        "bias" -> true,
        "p1_w" -> prevWord,
        "p1_c_n" -> digit.pattern.matcher(prevWord).matches(),
        "c" -> currentChar,
        "n2_c_uc" -> text.lift(context.offset + 2).map(_.isUpper).getOrElse("END"),
        "n1_c_whitespace" -> text.lift(context.offset + 1).map(_.isWhitespace).getOrElse("END")
      )
    }
  }

  trait TokenizerPotential extends TemplatedFeatureSumPotential {
    override lazy val truth = State(featureContexts.flatMap({case c => truthAt(c).map(splitVar(c) -> _)}).toMap)
    override def defaultArgmaxer(cookbook: ArgmaxRecipe[Potential]) =
      NaiveFactoredArgmaxerRecipe.argmaxer(this, cookbook)
  }

  /**
   * Predicts all character offsets at which there is a character that serves as punctuation.
   * @param doc the input document.
   * @return a sequence of character offsets that serve as
   */
  def predictPunctuation(doc: Doc) = {
    val predicted = predict(doc)
    candidates(doc).filter(c => predicted(splitVar(c))).map(_.offset)
  }

  /**
   * Based on the predicted punctuation, this returns a sequence of tokens.
   * @param doc the input document.
   * @return sequence of tokens.
   */
  def predictTokens(doc: Doc): Seq[Token] = {
    val src = source(doc)
    val splits = predictPunctuation(doc).toSet
    val result = new ArrayBuffer[Token]
    var start = 0
    for (index <- 0 until src.length) {
      if (src(index).isWhitespace) {
        if (index == 0 || !src(index - 1).isWhitespace) {
          result += Token(doc, start, index)
        }
        start = index + 1
      } else if (splits(index)) {
        result += Token(doc, start, index)
        start = index
      }
    }
    if (src.length > 0 && !src.last.isWhitespace) result += Token(doc, start, src.length)
    result
  }

}

/**
 * Tokenizer that works with the default nlp data structures.
 */
class DefaultTokenizer extends Tokenizer[Document] {
  val punctuation = Set(';', ',', '.')
  val punctuationStrings = punctuation.map(_.toString)

  def source(doc: Document) = doc.source()
  def truthAt(splitCandidate: SplitCandidate):Option[Boolean] = None
  def splitVar(splitCandidate: SplitCandidate) = BoolVar(splitCandidate)

  def candidates(doc: Document) = {
    val source = doc.source()
    for (index <- 0 until source.length; if (punctuation(source(index)))) yield SplitCandidate(doc, index)
  }

  def annotate(doc: Document) {
    val tokens = predictTokens(doc)
    doc.tokens := tokens.map(t => new nlp.Token().charBegin(t.beginChar).charEnd(t.endChar).word(t.word))
  }

}

/**
 * In this singleton we train the default tokenizer based on some Genia input corpus.
 */
object TrainDefaultTokenizer {

  val punctuation = Set(';', ',', '.')
  val punctuationStrings = punctuation.map(_.toString)

  /**
   * Takes genia pos text file input and returns an iterator over Doc objects which can be used to train the tokenizer.
   * @param geniaTxt input stream in
   * @return iterator of documents, one per sentence.
   */
  def loadGeniaDocs(geniaTxt: InputStream) = {
    val sentences = Util.streamIterator(geniaTxt, "====================\n")
    for ((sentence,index) <- sentences.zipWithIndex) yield {
      val tokens = sentence.split("\n")
      val newTokens = new ArrayBuffer[Token]
      var txt = new StringBuffer()
      var charPos = 0
      var goldSplits: Set[Int] = Set.empty
      for (token <- tokens) {
        val word = token.substring(0, token.lastIndexOf('/'))
        if (punctuationStrings(word)) {
          goldSplits = goldSplits + charPos
        } else if (charPos > 0) {
          txt.append(" ")
          charPos += 1
        }
        txt.append(word)
        newTokens += new Token().charBegin(charPos).charEnd(charPos + word.length).word(word)
        charPos += word.length
      }
      new Document().source(txt.toString).tokens(newTokens).name(index.toString)
    }
  }

  def goldSplits(doc:Document) = {
    doc.tokens().view.filter(t => punctuationStrings(t.word())).map(_.charBegin()).toSet
  }

  def main(args: Array[String]) {

    val inputFile = "/Users/riedelcastro/corpora/genia/GENIAcorpus3.02.pos.txt"
    val docs = loadGeniaDocs(new FileInputStream(inputFile)).toSeq
    val (trainSet, testSet) = docs.splitAt(docs.size / 2)

    val goldOffsets = new collection.mutable.HashMap[Document,Set[Int]]

    val tokenizer = new DefaultTokenizer {
      override def truthAt(splitCandidate: SplitCandidate) = {
        val splits = goldOffsets.getOrElseUpdate(splitCandidate.doc,goldSplits(splitCandidate.doc))
        Some(splits(splitCandidate.offset))
      }
    }

    //pre-collect gold features, but only use those from non-splitting punctuation
    val filter = new ParameterVector
    for (doc <- docs) {
      val pot = tokenizer.potential(doc)
      val truth = pot.truth
      val feats = pot.features(truth).filterKeys(k => {
        k match {
          case f: Feat if (f.last == 'false) => true
          case x => false
        }
      })
      filter.add(feats, 1.0)
    }

    filter.save(new PrintStream("/tmp/filter.weights"))

    val learner = new OnlineLearner[Document] with PerceptronUpdate {
      val template = tokenizer
      def instances = trainSet
      override def featureDelta(potential: PotentialType, gold: State, guess: State) =
        super.featureDelta(potential, gold, guess).filterKeys(f => filter.isDefinedAt(f))
    }

    learner.epochs = 2
    learner.train()

    for (doc <- testSet) {
      val result = tokenizer.predictPunctuation(doc)
      val tokens = tokenizer.predictTokens(doc)
      println(doc.source())
      println(tokens.map(_.word).mkString(" "))
      println(goldSplits(doc))
      println(result)
    }

    tokenizer.weights.save(new PrintStream("/tmp/tokenizer.weights"))

  }
}

/**
 * This code simply loads a previously trained tokenizer and applies it to some example text.
 */
object DefaultTokenizerTest {
  def main(args: Array[String]) {
    val doc = new Document().source("This is a test document (i.e. a document to test). Yeah.")
    val tokenizer = new DefaultTokenizer
    tokenizer.weights.load(new FileInputStream("/tmp/tokenizer.weights"))
    tokenizer.annotate(doc)
    println(doc.toJSON)

  }
}


