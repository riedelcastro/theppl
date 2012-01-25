package com.github.riedelcastro.theppl

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

      class EdgePotential(x: Var, y: Var) extends LinearModel {
        def features(state: State) = new ParameterVector(Feat(state(x), state(y)))
        def hidden = IndexedSeq(A, B)
        def argmax(penalties: Messages) = null
        def expectations(penalties: Messages) = null
        def marginalize(penalties: Messages) = null
        def weights = new ParameterVector()
      }
      val AB = new EdgePotential(A, B)
      val BC = new EdgePotential(B, C)
      val BD = new EdgePotential(B, D)
      AB.weights(Feat('x1, 'x1)) = 1.0
      BC.weights(Feat('x1, 'x1)) = 1.0
      BD.weights(Feat('x1, 'x1)) = 1.0

      val sum = new LinearSumModel with FiniteSupportModel{
        def opaqueArgs = IndexedSeq(AB)
        def linearArgs = IndexedSeq(BC, BD)
        def argmax(penalties: Messages) = null
        def expectations(penalties: Messages) = null
        def marginalize(penalties: Messages) = null
        def weights = null
      }

      val brute = BruteForceExpectator.expectator(sum)
      val bp = SumProductBPRecipe.expectator(sum, DefaultExpectators)


    }
  }

}