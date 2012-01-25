package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
class SumProductBPSpec extends ThePPLSpec {

  describe("A Sum Product BP implementation") {
    it("should match results of exact inference on a tree") {
      val values = IndexedSeq('x1, 'x2)
      case class Var(name: String) extends Variable[Symbol] {
        def domain = values
      }
      val A = Var("A")
      val B = Var("B")
      val C = Var("C")
      val D = Var("D")
      val AB = new LinearModel {
        def features(state: State) = new ParameterVector(Seq(Feat(A, state(A)), Feat(B, state(B))))
        def hidden = IndexedSeq(A,B)
        def argmax(penalties: Messages) = null
        def expectations(penalties: Messages) = null
        def marginalize(penalties: Messages) = null
        def weights = new ParameterVector(Seq(Feat(A, state(A)), Feat(B, state(B))))
      }

    }
  }

}