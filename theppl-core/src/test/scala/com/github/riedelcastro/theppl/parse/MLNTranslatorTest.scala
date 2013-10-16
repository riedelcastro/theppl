package com.github.riedelcastro.theppl.parse

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

/**
 * Created by larysa  08.07.13
 */
class MLNTranslatorTest extends FunSpec with MustMatchers {
  describe("MLN translator") {
    it("loads MLN specification") {
      val MLN = new MLNEmbeddedTranslator

      val mln_file = "theppl-core/src/test/data/mln/social-network/smoking.mln"
      MLN.translateMLNFromFile(mln_file)

      val db_file = "theppl-core/src/test/data/mln/social-network/smoking-train.db"
      MLN.translateDatabaseFromFile(db_file)

      println("atoms  ")
      val atoms = MLN.atoms
      atoms.foreach(x => println(x))

      println("formulae  ")
      val formulae = MLN.formulae2
      formulae.foreach(x => println(x))

      val worldState = MLN.state2
      worldState.foreach(x => println(x))
    }


  }

}
