package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.infer.Argmaxer
import com.github.riedelcastro.theppl.LabelVar
import com.github.riedelcastro.theppl.VecVar
import com.github.riedelcastro.theppl.StringVar
import learn.LinearLearner

/**
 * @author Sebastian Riedel
 */
object PropositionalExample {

  import TermImplicits._

  def main(args: Array[String]) {
    val label = LabelVar('label,Seq('Noun,'Verb,'Det))
    val word = StringVar('word)
    val index = new Index
    val weights = VecVar('weights)

    val feats = index('f,word,label) --> 1.0
    val model = Loglinear(feats,weights)

    val training = Seq(
      State(Map(word -> "water", Target(label) -> 'Noun)),
      State(Map(word -> "walk", Target(label) -> 'Verb))
    )

    val trained = LinearLearner.learn(model)(training)

    val result = Argmaxer(model | weights -> trained | word -> "walk").predict

    println(result)

    println(trained)
    println(index)

  }
}


