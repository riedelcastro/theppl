package com.github.riedelcastro.theppl

import math._

trait ExampleModels {
  case class Var(index: Int, domain: Seq[Int]) extends Variable[Int]

  /**
   * A model with integer variables that scores each state
   * by summing up the integer values associated with the variables.
   */
  abstract class ExampleModel(varCount: Int = 3, domainSize: Int = 3) extends Model {
    override lazy val hidden = Range(0, varCount).map(V(_))
    lazy val domain = Range(0, domainSize)
    def V(index: Int) = Var(index, domain)
  }

  /**
   * Scores by summing up the integer values associated with the variables.
   */
  trait SumScore extends Model {
    this: ExampleModel =>
    def score(state: State) = hidden.map(v => state(v)).sum
  }

  /**
   * A linear model with one feature per (variable-value) combination for all hidden variables. The weight
   * vector is 1.0 iff the value associated to the variable corresponds to the index of the variable.
   */
  trait LinearScore extends LinearModel {
    this: ExampleModel =>

    def features(state: State) =
      ParameterVector.fromPairIterable(hidden.map(v => v -> state(v)))
    def weights =
      ParameterVector.fromPairIterable(hidden.map(v => v -> v.index))
  }


  /**
   * A message that penalizes the same value for each variable.
   */
  def exampleMessage(valueToPenalize: Int = 2, penalty: Double = -10) = {
    val message = Messages.fromFunction({
      case (Var(_, _), x) if (valueToPenalize == x) => penalty
      case _ => 0.0
    })
    message
  }
}
/**
 * @author sriedel
 */
class BruteForceArgmaxerSpec extends ThePPLSpec with ExampleModels {


  describe("A BruteForceArgmaxer") {
    it("should calculate the exact argmax") {
      val model = new ExampleModel() with SumScore
      val argmaxer = Argmaxer(model)
      import model._
      val message = exampleMessage()
      val result = argmaxer.argmax(message)
      val expected = State.fromFunction(Map(V(0) -> 1, V(1) -> 1, V(2) -> 1))
      model.hidden.foreach(v => result.state(v) must be(expected(v)))
    }
  }

}

class BruteForceMarginalizerSpec extends ThePPLSpec with ExampleModels {
  describe("A BruteForceMarginalizer") {
    it("should calculate exact marginals") {
      val m = new ExampleModel(2, 2) with SumScore
      val marginalizer = new BFMarginalizer {val model = m}
      val message = exampleMessage(1, -1)
      val result = marginalizer.marginalize(message)

      result.logZ must be(log(4.0) plusOrMinus eps)

      for (v <- m.hidden; value <- v.domain)
        result.logMarginals(v, value) must be(log(0.5) plusOrMinus eps)

    }
  }
}

class BruteForceExpectationCalculatorSpec extends ThePPLSpec with ExampleModels {
  describe("A BruteForceExpectator") {
    it("should calculate exact expectations in linear models") {
      val model = new ExampleModel(2, 2) with LinearScore
      import model._
      val message = exampleMessage(1, -1)
      val expectator = BruteForceExpectator.expectator(model)
      val result = expectator.expectations(message)
      val Z = exp(1) + exp(1) + exp(-1) + exp(-1)

      result.logZ must be(log(Z) plusOrMinus eps)

      val expectations = result.featureExpectations
      expectations(Feat(V(0), 0)) must be((exp(1) + exp(1)) / Z plusOrMinus eps)
      expectations(Feat(V(0), 1)) must be((exp(-1) + exp(-1)) / Z plusOrMinus eps)
      expectations(Feat(V(1), 0)) must be((exp(1) + exp(-1)) / Z plusOrMinus eps)
      expectations(Feat(V(1), 1)) must be((exp(1) + exp(-1)) / Z plusOrMinus eps)

      val marginals = result.logMarginals
      exp(marginals(V(0), 0)) must be((exp(1) + exp(1)) / Z plusOrMinus eps)
      exp(marginals(V(0), 1)) must be((exp(-1) + exp(-1)) / Z plusOrMinus eps)
      exp(marginals(V(1), 0)) must be((exp(1) + exp(-1)) / Z plusOrMinus eps)
      exp(marginals(V(1), 1)) must be((exp(1) + exp(-1)) / Z plusOrMinus eps)


    }
  }

}