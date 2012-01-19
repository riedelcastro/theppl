package com.github.riedelcastro.theppl

import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec

/**
 * @author sriedel
 */
class BruteForceModelSpec extends Spec with MustMatchers {
  describe("A BruteForceModelSpec") {
    it("should calculate the exact argmax") {
      case class V(index: Int) extends Variable[Int]
      val domain = Seq(1, 2, 3)
      val variables = Range(0, 3).map(V(_))
      val message = Message({
        case (V(i),3) => -10
        case _ => 0.0
      })
      val model = new BruteForceModel {
        def score(state: State) = variables.map(v => state(v)).sum
        def restrictions = variables.map(Restriction(_, domain))
      }

      val result = model.argmax(message)
      val expected = State(Map(V(0) -> 2, V(1) -> 2, V(2) -> 2))
      variables.foreach(v => result(v) must be (expected(v)))

    }
  }

}