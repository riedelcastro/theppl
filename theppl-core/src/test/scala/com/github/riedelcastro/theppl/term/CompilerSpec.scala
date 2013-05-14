package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl.{State, IntVar, ThePPLSpec}

/**
 * @author Sebastian Riedel
 */
class CompilerSpec extends ThePPLSpec {

  import TermImplicits._

  describe("A Term Compiler") {

    ignore("should compile variable terms") {
      val x = IntVar('x)
      val compiled = TermCompiler.compile(x)
      val state = State(Map(x -> 1))
      compiled.eval(state) must be(x.eval(state))
    }

    ignore("should compile infix operations") {
      val x = IntVar('x)
      val term = x + x
      val compiled = TermCompiler.compile(term)
      val state = State(Map(x -> 1))
      compiled.eval(state) must be(term.eval(state))
    }

    ignore("should compile an indexed term") {
      val index = new Index
      val term = index('f, 'A)
      val compiled = TermCompiler.compile(term)
      compiled.eval(State.empty) must be(term.eval(State.empty))
    }

    ignore("should compile a singleton vector") {
      val term = 1 --> 1.0
      val compiled = TermCompiler.compile(term)
      val result = compiled.eval(State.empty).get
      val expected = term.eval(State.empty).get
      (result === expected) must be(true)
    }

    ignore("should compile a sum of singleton vectors") {
      val term = (1 --> 1.0) + (1 --> 2.0) + (2 --> 3.0)
      val compiled = TermCompiler.compile(term)
      val result = compiled.eval(State.empty).get
      val expected = term.eval(State.empty).get
      result must be(expected)
    }

    ignore("should compile a dot product") {
      val term = (1 --> 1.0) dot (1 --> 2.0)
      val compiled = TermCompiler.compile(term)
      val result = compiled.eval(State.empty).get
      val expected = term.eval(State.empty).get
      result must be(expected)
    }



  }

}