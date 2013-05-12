package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl.{State, VecVar, ThePPLSpec}

/**
 * @author Sebastian Riedel
 */
class UnrollerSpec extends ThePPLSpec {

  import TermImplicits._

  describe("An Unroller") {
    it("should unroll a MLN into a sequence of grouped ground formulae") {
      val Persons = Dom('persons, Seq('Anna, 'Peter))
      val cancer = 'cancer := Persons -> Bool
      val smokes = 'smokes := Persons -> Bool
      val index = new Index()
      val f1 = vecSum { for (p <- Persons) yield index('cancer_bias) --> I { cancer(p) } }
      val f2 = vecSum { for (p <- Persons) yield index('smoking_is_bad) --> I { smokes(p) ==> cancer(p) } }
      val x = State(Map(smokes('Anna) -> true, smokes('Peter) -> false))
      val w = VecVar('weights)
      val mln = Loglinear((f1 + f2) | x, w)

      val unrolled = Unroller.unrollAndGroupLogLinear(mln)

      val t1 = { index('cancer_bias) --> I { cancer('Anna) } } | x
      val t2 = { index('cancer_bias) --> I { cancer('Peter) } } | x
      val t3 = { index('smoking_is_bad) --> I { smokes('Anna) ==> cancer('Anna) } } | x
      val t4 = { index('smoking_is_bad) --> I { smokes('Peter) ==> cancer('Peter) } } | x
      val expected = Set(Set(t1, t3), Set(t2, t4))
      val Loglinear(VecAddN(SeqTerm(Seq(a1, a2))),_,_) = unrolled(0)
      val Loglinear(VecAddN(SeqTerm(Seq(a3, a4))),_,_) = unrolled(1)
      val actual = Set(Set(a1,a2),Set(a3,a4))

      actual must be (expected)




    }
  }


}
