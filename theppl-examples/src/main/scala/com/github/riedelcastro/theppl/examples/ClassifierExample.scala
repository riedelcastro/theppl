package com.github.riedelcastro.theppl.examples

import com.github.riedelcastro.theppl.util.Util
import io.Source
import com.github.riedelcastro.theppl._

/**
 * @author sriedel
 */
object ClassifierExample {

  def main(args: Array[String]) {

    val chunks = Seq("O") ++ (for (bi <- Seq("B-","I-"); t <- Seq("VP","NP","PP")) yield bi + t)
    case class Token(index:Int, word:String, tag:String, chunk:String) extends Var(chunks)
    val stream = Util.getStreamFromClassPathOrFile("com/github/riedelcastro/theppl/datasets/conll2000/train.txt")
    val indexedLines = Source.fromInputStream(stream).getLines.take(10).filter(_ != "").zipWithIndex
    val tokens = for ((line,index) <- indexedLines.toSeq; Array(word,tag,chunk) = line.split("\\s+")) yield
      Token(index,word,tag,chunk)
    val lifted = tokens.lift
    val training = for (token <- tokens) yield new TrainingInstance(token,new SingletonState(token,token.chunk))
    val contextFeats = (token:Token) => new ParameterVector(Seq(token.tag, lifted(token.index-1).map(_.tag)))
    def labelFeats = (label:String) => new ParameterVector(Set(label) ++ label.split("-"))
    val classifier = new Classifier(contextFeats,labelFeats) with OnlineLearner
    println(tokens.mkString("\n"))
  }

  def main2(args: Array[String]) {
    val labels = Seq("NP", "PP")
    class Token(val word: String, val index: Int) extends Var(labels)
    val tokens = for ((word, index) <- Seq("A", "man", "went", "to", "the", "park") zipWithIndex) yield
      new Token(word, index)
    val classifier = new Classifier((t:Token) => new ParameterVector(Seq(t.word)),
                                    (l:String) => new ParameterVector(Seq(l)))
    val predictions = for (token <- tokens) yield classifier.classify(token)
    println(predictions)
  }
}