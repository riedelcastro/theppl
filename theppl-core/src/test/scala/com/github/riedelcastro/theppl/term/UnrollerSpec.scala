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
      val mln = Loglinear((f1 + f2) |x ,w)
      //val mln = Loglinear(f1 + f2,w)

      val unrolled = Unroller.unrollAndGroupLogLinear(mln)

      for (t <- unrolled) {
        println(t.variables.mkString(","))
        println(t)
      }


    }
  }


}
