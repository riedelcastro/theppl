package com.github.riedelcastro.theppl

import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec

trait ExampleModels {
  case class V(index: Int) extends Variable[Int]

  /**
   * A model with integer variables that scores each state
   * by summing up the integer values associated with the variables.
   */
  def exampleModel(varCount: Int = 3, domainSize: Int = 3) = {
    val variables = Range(0, varCount).map(V(_))
    val domain = Range(0, domainSize)
    val model = new BruteForceArgmaxer with BruteForceMarginalizer {
      def score(state: State) = variables.map(v => state(v)).sum
      def restrictions = variables.map(Restriction(_, domain))
    }
    model
  }

  /**
   * A message that penalizes the same value for each variable.
   */
  def exampleMessage(valueToPenalize: Int = 2, penalty: Double = -10) = {
    val message = Message.fromFunction({
      case (V(_), x) if (valueToPenalize == x) => penalty
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
      val model = exampleModel()
      val message = exampleMessage()
      val result = model.argmax(message)
      val expected = State.fromFunction(Map(V(0) -> 1, V(1) -> 1, V(2) -> 1))
      model.hidden.foreach(v => result.state(v) must be(expected(v)))
    }
  }

}

class BruteForceMarginalizerSpec extends ThePPLSpec with ExampleModels {
  describe("A BruteForceMarginalizer") {
    it("should calculate exact marginals") {
      val model = exampleModel(2, 2)
      val message = exampleMessage(1, -1)
      val result = model.marginalize(message)

      result.partitionFunction must be(4.0 plusOrMinus eps)

      for (Restriction(v, d) <- model.restrictions;
           value <- d)
        result.marginals(v, value) must be(0.5 plusOrMinus eps)

    }
  }
}