package com.github.riedelcastro.theppl.infer

import math._
import com.github.riedelcastro.theppl._

trait ExamplePotentials {
  case class Var(index: Int, domain: Seq[Int]) extends Variable[Int]

  /**
   * A potential with integer variables that scores each state
   * by summing up the integer values associated with the variables.
   */
  abstract class ExamplePotential(varCount: Int = 3, domainSize: Int = 3) extends Potential {
    override lazy val hidden = Range(0, varCount).map(V(_))
    lazy val domain = Range(0, domainSize)
    def V(index: Int) = Var(index, domain)
  }

  /**
   * Scores by summing up the integer values associated with the variables.
   */
  trait SumScore extends Potential {
    this: ExamplePotential =>
    def score(state: State) = hidden.map(v => state(v)).sum
  }

  /**
   * A linear potential with one feature per (variable-value) combination for all hidden variables. The weight
   * vector is 1.0 iff the value associated to the variable corresponds to the index of the variable.
   */
  trait LinearScore extends LinearPotential {
    this: ExamplePotential =>

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
class BruteForceArgmaxerSpec extends ThePPLSpec with ExamplePotentials {


  describe("A BruteForceArgmaxer") {
    it("should calculate the exact argmax") {
      val potential = new ExamplePotential() with SumScore
      val argmaxer = Argmaxer(potential)
      import potential._
      val message = exampleMessage()
      val result = argmaxer.argmax(message)
      val expected = State.fromFunction(Map(V(0) -> 1, V(1) -> 1, V(2) -> 1))
      potential.hidden.foreach(v => result.state(v) must be(expected(v)))
    }
  }

}

class BruteForceMarginalizerSpec extends ThePPLSpec with ExamplePotentials {
  describe("A BruteForceMarginalizer") {
    it("should calculate exact marginals") {
      val m = new ExamplePotential(2, 2) with SumScore
      val marginalizer = new BFMarginalizer {val potential = m}
      val message = exampleMessage(1, -1)
      val result = marginalizer.marginalize(message)

      result.logZ must be(log(4.0) plusOrMinus eps)

      for (v <- m.hidden; value <- v.domain)
        result.logMarginals(v, value) must be(log(0.5) plusOrMinus eps)

    }
  }
}

class BruteForceExpectationCalculatorSpec extends ThePPLSpec with ExamplePotentials {
  describe("A BruteForceExpectator") {
    it("should calculate exact expectations in linear potentials") {
      val potential = new ExamplePotential(2, 2) with LinearScore
      import potential._
      val message = exampleMessage(1, -1)
      val expectator = BruteForceExpectator.expectator(potential)
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