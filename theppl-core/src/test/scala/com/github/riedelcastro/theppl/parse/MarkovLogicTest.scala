package com.github.riedelcastro.theppl.parse

import org.scalatest.FunSpec

/**
 * Created by larysa  05.09.13
 */
class MarkovLogicTest extends FunSpec {
  describe("markov logic in action") {
    it("loads MLN specification and performs inference") {
      val mln_file = "theppl-core/src/test/data/mln/social-network/smoking.mln"
      val db_file = "theppl-core/src/test/data/mln/social-network/smoking-train.db"
      val result_file = "theppl-core/src/test/data/mln/social-network/smoking.result"

      /* simulating the Alchemy Markov Logic CLI commands...*/
      val markovLogic = new MarkovLogic
      markovLogic.infer.input(mln_file).result(result_file).evidence(db_file).query(Seq("Cancer"))
    }
  }

}
