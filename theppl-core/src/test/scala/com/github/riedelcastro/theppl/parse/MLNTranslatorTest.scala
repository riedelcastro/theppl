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
      val atoms = MLN.atoms
      atoms.foreach(x => println(x))

      val formulae = MLN.formulae
      formulae.foreach(x => println(x))

      val db_file = "theppl-core/src/test/data/mln/social-network/smoking-train.db"
      MLN.translateDatabaseFromFile(db_file)
      val state = MLN.state
      state.foreach(x => println(x))


    }


  }

}
