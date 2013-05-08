package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import infer.{BruteForceArgmaxer, Argmaxer}
import com.github.riedelcastro.theppl.VectorVar
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.Loglinear


object Learner {

  import TermImplicits._

  def learn(model: Loglinear)(instances: Seq[State]): Vec = {
    val weights = new DenseVec(1000)
    for (epochs <- 0 until 2) {
      for (instance <- instances) {
        println(model.substitute(Substitution(Seq(model.weights), Seq(weights))))
        val conditioned = (model | model.weights -> weights) | instance
        val argmaxer = Argmaxer(conditioned)
        val guess = argmaxer.argmax().state
        val gold = instance.target
        val guessFeats = model.features.eval(guess).get
        val goldFeats = model.features.eval(gold).get
        weights.add(goldFeats, 1.0)
        weights.add(guessFeats, -1.0)
      }
    }
    weights
  }
}

trait Learner[Context] extends HasTemplate[Context] {

  def instances: Seq[Context]
  def train()
}
