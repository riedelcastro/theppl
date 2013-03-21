package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.infer.Argmaxer
import com.github.riedelcastro.theppl.LabelVar
import com.github.riedelcastro.theppl.VecVar
import com.github.riedelcastro.theppl.StringVar

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

    val trained = LearnerNew.learn(model)(training)

    val result = Argmaxer(model | weights -> trained | word -> "walk").predict

    println(result)

    println(trained)
    println(index)

  }
}

object LearnerNew {

  import TermImplicits._

  def learn(model: Loglinear)(instances: Seq[State]): Vec = {
    val weights = new DenseVec(1)
    for (epochs <- 0 until 2) {
      for (instance <- instances) {
        //create a conditioned term that adds
//        println(model.substitute(Substitution(Seq(model.weights), Seq(weights))))
        val conditioned = (model | model.weights -> weights) | instance
        val argmaxer = Argmaxer(conditioned)
        val guess = argmaxer.argmax().state
        val gold = instance.target
        val guessFeats = (model.features | instance).eval(guess).get
        val goldFeats = (model.features | instance).eval(gold).get
        weights.add(goldFeats, 1.0)
        weights.add(guessFeats, -1.0)
      }
    }
    weights
  }
}
