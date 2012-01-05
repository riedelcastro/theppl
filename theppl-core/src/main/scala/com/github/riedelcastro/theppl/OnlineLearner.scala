package com.github.riedelcastro.theppl

/**
 * An OnlineLearner iterates over the training set and updates
 * weights after processing each instance.
 * @author sriedel
 */
trait OnlineLearner extends LinearModule with Learner {
  self: UpdateRule =>

  var epochs: Int = 2

  def train(instances: Seq[Instance[Context]]) {
    for (epoch <- 0 until epochs) {
      for (instance <- instances) {
        val gold = instance.gold
        val model = self.model(instance.context, instance.observation)
        val guess = model.predict
        updateRule(model, gold, guess)
      }
    }
  }

}

/**
 * The UpdateRule of an Online Learner defines how the prediction (guess) and
 * the gold data are used to update the weights.
 */
trait UpdateRule {
  this: OnlineLearner =>
  def updateRule(model: ModelType, gold: State, guess: State)
}

/**
 * The PerceptronUpdate simply adds the feature delta of gold and guess.
 */
trait PerceptronUpdate extends UpdateRule {
  this: OnlineLearner =>

  var learningRate = 1.0

  def updateRule(model: ModelType, gold: State, guess: State) {
    val delta = model.featureDelta(gold, guess)
    weights.add(delta, learningRate)
  }


}

trait Learner extends Module {
  def train(instances: Seq[Instance[Context]])
}

class Corpus(val module: Module)

case class Instance[C](context: C, gold: State, observation: State = State.empty) {

}

