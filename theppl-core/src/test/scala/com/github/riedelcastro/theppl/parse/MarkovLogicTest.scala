package com.github.riedelcastro.theppl.parse

import org.scalatest.FunSpec
import java.util
import com.mongodb.DBCollection
import com.github.riedelcastro.theppl.util.LoanResourceManager
import java.nio.file.Paths

/**
 * Created by larysa  05.09.13
 */
class MarkovLogicTest extends FunSpec {
  describe("markov logic in action") {
    it("loads MLN specification and performs inference") {
      val mln_file = "theppl-core/src/test/data/mln/social-network/smoking.mln"
      val db_file = "theppl-core/src/test/data/mln/social-network/smoking-train.db"
      val result_file = "theppl-core/src/test/data/mln/social-network/smoking.result"

      /* prepare db for */
      LoanResourceManager.withMongoConnector( Paths.get(db_file).toFile.getName) {
        db => {
          val colls = db.getCollectionNames
          val iterator: util.Iterator[String] = colls.iterator()
          while (iterator.hasNext) {
            val name = iterator.next()
            println("coll name = " + name)
            if (name != "system.indexes") {
              val collection: DBCollection = db.getCollection(name)
              collection.drop()
            }
          }
        }
      }
      /* simulating the Alchemy Markov Logic CLI commands...*/
      val markovLogic = new MarkovLogic
      markovLogic.infer.input(mln_file).result(result_file).evidence(db_file).query(Seq("Cancer"))
    }
  }

}
