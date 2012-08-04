package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.{Potential, BoolVar, ThePPLSpec}

/**
 * @author riedelcastro
 */
class NaiveFactoredArgmaxerSpec extends ThePPLSpec {

  describe("A NaiveFactoredArgmaxer") {
    it("should optimize each factor in a sum in isolation") {
      val vars = Seq(BoolVar(0), BoolVar(1), BoolVar(2))
      val tables = vars.map(v => Potential.table(Seq(v),Map(Seq(true) -> -1.0, Seq(false) -> 1.0)))
      val pot = Potential.sum(tables)
      val argmaxer = NaiveFactoredArgmaxerRecipe.argmaxer(pot)
      val result = argmaxer.argmax()
      for (v <- vars) result.state(v) must be (false)
      result.score must be (3.0)
    }
  }

}
