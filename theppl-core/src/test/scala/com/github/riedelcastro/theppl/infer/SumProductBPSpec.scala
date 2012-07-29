package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl._


/**
 * @author sriedel
 */
class SumProductBPSpec extends ThePPLSpec {

  describe("A Sum Product BP implementation") {
    it("should match results of exact inference on a tree") {
      val values = IndexedSeq('x1, 'x2)
      case class Var(name: Symbol) extends Variable[Symbol] {
        def domain = values
      }
      val A = Var('A)
      val B = Var('B)
      val C = Var('C)
      val D = Var('D)

      val vars = Seq(A, B, C, D)

      class EdgePotential(x: Var, y: Var) extends LinearPotential {
        def features(state: State) = ParameterVector(state(x) -> state(y))
        def hidden = IndexedSeq(x, y)
        val weights = new ParameterVector()
        override def toString = (x, y).toString()
      }
      val AB = new EdgePotential(A, B)
      val BC = new EdgePotential(B, C)
      val BD = new EdgePotential(B, D)
      AB.weights('x1 -> 'x1) = 1.0
      BC.weights('x1 -> 'x1) = 1.0
      BD.weights('x1 -> 'x1) = 1.0

      val sum = new FeatureSumPotential {

        def featureArgs = IndexedSeq(AB, BC, BD)
        def otherArgs = IndexedSeq.empty
      }

      val brute = BruteForceExpectator.expectator(sum)
      val bp = SumProductBPRecipe.expectator(sum)

      val bfExp = brute.expectations(Messages.empty) //todo: an idea bug requires the explicit parameter
      val bpExp = bp.expectations()

      (bfExp.featureExpectations - bpExp.featureExpectations).norm1 must be(0.0 plusOrMinus eps)

      for (v <- vars)
        (bfExp.logMarginals.message(v) - bpExp.logMarginals.message(v)).norm1 must be(0.0 plusOrMinus eps)

      bfExp.logZ must be(bpExp.logZ plusOrMinus eps)


    }

    it("should calculate the Bethe Energy for a cycle") {
      val values = IndexedSeq('x1, 'x2)
      case class Var(name: Symbol) extends Variable[Symbol] {
        def domain = values
      }
      val A = Var('A)
      val B = Var('B)
      val C = Var('C)

      val vars = Seq(A, B, C)

      class EdgePotential(x: Var, y: Var) extends LinearPotential {
        def features(state: State) = ParameterVector(state(x) -> state(y))
        def hidden = IndexedSeq(x, y)
        val weights = new ParameterVector()
        override def toString = (x, y).toString()
      }
      val AB = new EdgePotential(A, B)
      val BC = new EdgePotential(B, C)
      val CA = new EdgePotential(C, A)
      AB.weights('x1 -> 'x1) = 1.0
      BC.weights('x1 -> 'x1) = 1.0
      CA.weights('x1 -> 'x1) = 1.0

      val sum = new FeatureSumPotential {

        def featureArgs = IndexedSeq(AB, BC, CA)
        def otherArgs = IndexedSeq.empty
      }

      val brute = BruteForceExpectator.expectator(sum)
      val bp = SumProductBPRecipe.expectator(sum)

      val bpExp = bp.expectations()
      val bfExp = brute.expectations(Messages.empty) //todo: an idea bug requires the explicit parameter

      //calculate bethe
//      var bethe = 0.0
//      val edges = Seq(AB,BC,CA)
//      for (edge <- edges) {
//        bethe += bpExp
//      }
      println(bfExp.logZ)
      println(bpExp.logZ)


//      (bfExp.featureExpectations - bpExp.featureExpectations).norm1 must be(0.0 plusOrMinus eps)
//
//      for (v <- vars)
//        (bfExp.logMarginals.message(v) - bpExp.logMarginals.message(v)).norm1 must be(0.0 plusOrMinus eps)
//
//      bfExp.logZ must be(bpExp.logZ plusOrMinus eps)


    }

  }

}