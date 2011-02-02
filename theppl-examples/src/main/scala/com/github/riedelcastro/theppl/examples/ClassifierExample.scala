package com.github.riedelcastro.theppl.examples

import com.github.riedelcastro.theppl.{Var, ParameterVector, Classifier, Module}

/**
 * @author sriedel
 */
object ClassifierExample {
  def main(args: Array[String]) {
    class Token(val word: String, val index: Int)
    val labels = Seq("NP", "PP")
    val tokens = for ((word, index) <- Seq("A", "man", "went", "to", "the", "park") zipWithIndex) yield
      new Token(word, index)
    val classifier = new Classifier(labels, (t: Token) => new ParameterVector(Seq(t.word)))
    val factors = tokens.map(classifier.factor(_))
    println(factors(0).argmax(null).get(factors(0).variable))
  }
}