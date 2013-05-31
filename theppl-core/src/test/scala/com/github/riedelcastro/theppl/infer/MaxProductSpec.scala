package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.{LabelVar, Table, Variable, ThePPLSpec}
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class MaxProductSpec extends ThePPLSpec {

  import com.github.riedelcastro.theppl.term.TermImplicits._

  describe("A MaxProduct algorithm") {
    it("should return the true MAP solution in a tree") {
      val random = new Random(20)
      def table(arg1: Variable[Any], arg2: Variable[Any]) = Table(Seq(arg1, arg2), { case _ => random.nextGaussian() })
      val Domain = Seq(1, 2)
      val Seq(a, b, c, d) = Seq('A, 'B, 'C, 'D).map(LabelVar(_, Domain))
      for (i <- 0 until 10) {
        val terms = Seq(a -> b, b -> c, c -> d).map(p => table(p._1, p._2))
        val Seq(ab, bc, cd) = terms
        val model = ab + bc + cd
        val mp = new MaxProduct(model)
        val mpSolution = mp.maxMarginals().messages.argmaxState
        val bfSolution = model.argmax().state
        mpSolution must be (bfSolution)
      }

    }
    it("should return the true MAP solution in a single loop (see Weiss 2000)") {
      val random = new Random(20)
      def table(arg1: Variable[Any], arg2: Variable[Any]) = Table(Seq(arg1, arg2), { case _ => random.nextGaussian() })
      val Domain = Seq(1, 2)
      val Seq(a, b, c) = Seq('A, 'B, 'C).map(LabelVar(_, Domain))
      for (i <- 0 until 10) {
        val terms = Seq(a -> b, b -> c, c -> a).map(p => table(p._1, p._2))
        val Seq(ab, bc, ca) = terms
        val model = ab + bc + ca
        val mp = new MaxProduct(model)
        val mpSolution = mp.maxMarginals().messages.argmaxState
        val bfSolution = model.argmax().state
        mpSolution must be (bfSolution)
      }

    }

  }

}
