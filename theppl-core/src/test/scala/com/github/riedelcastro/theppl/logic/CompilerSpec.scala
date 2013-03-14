package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{State, IntVar, ThePPLSpec}

/**
 * @author Sebastian Riedel
 */
class CompilerSpec extends ThePPLSpec{

  import LogicImplicits._

  describe("A Term Compiler") {

    it("should compile variable terms") {
      val x = IntVar('x)
      val compiled = TermCompiler.compile(x)
      val state = State(Map(x -> 1))
      compiled.eval(state) must be (x.eval(state))
    }

    it("should compile infix operations") {
      val x = IntVar('x)
      val term = x + x
      val compiled = TermCompiler.compile(term)
      val state = State(Map(x -> 1))
      compiled.eval(state) must be (term.eval(state))
    }


  }

}