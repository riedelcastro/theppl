package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl.{BoolVar, State, IntVar, ThePPLSpec}

/**
 * @author Sebastian Riedel
 */
class ConditionedSpec extends ThePPLSpec {

  import TermImplicits._

  describe("An Conditioned Term") {
    it("should replace free variables with the values in the conditioned state") {
      val x = IntVar('x)
      (x | x -> 5).eval(State.empty) must be (Some(5))
    }

    it("should replace a single ground atom if all arguments of a predicate are defined given the condition") {
      val pred = 'pred := (Bool, Bool) -> Bool
      val a1 = BoolVar('a1)
      val a2 = BoolVar('a2)
      val term = a1 && pred(a1,a2 && a1) | State(Map(a1 -> true, a2 -> false))
      val vars = term.variables
      vars must be (Set(GroundAtom2(pred,true,false)))
    }

  }

}
