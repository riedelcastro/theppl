package com.github.riedelcastro.theppl.parse

import org.scalatest._
import org.scalatest.matchers.MustMatchers

/**
 * Tests parsing the existing MLNs to theppl.
 */
class MLNParserTest extends FunSpec with MustMatchers {

  describe("MLNParser") {
    it("just logs parsing") {

      val mln_exp = MLNParser.expression

      // ;-)  haha, really like this one.
      val test = "10.0 Same(+hallo,!po) /* Hallo\nDu Igel */ ^ \n (Popel(du,igel)) => Same(du, nuss)"
      val parser = MLNParser.parse(mln_exp, test)
      println("parser = " + parser)

      val include = "#include \"Blah.mln\""
      val parse_include = MLNParser.parse(mln_exp, include)
      println("parse_include = " + parse_include)

      val mln_dir = "theppl-core/src/test/data/mln/social-network/"

      val mln_file = scala.io.Source.fromFile(mln_dir + "smoking.mln")
      def nonMLNElements(x: String): Boolean = {
        /*Methods with empty parameter lists are, by convention, evaluated for their side-effects.
         Methods without parameters are assumed to be side-effect free. That's the convention. */
        !((x startsWith "//") || (x isEmpty))
      }
      val mln_as_list = mln_file.getLines().filter(nonMLNElements(_)).map(MLNParser.parse(mln_exp, _)).foreach(x => println("parsed mln: " + x))
      mln_file.close()

      val db_train_file = scala.io.Source.fromFile(mln_dir + "smoking-train.db")
      val db_train_as_list = db_train_file.getLines().filter(nonMLNElements(_)).map(MLNParser.parse(mln_exp, _)).foreach(x => println("parsed db: " + x))
      db_train_file.close()

      val db_test_file = scala.io.Source.fromFile(mln_dir + "smoking-train.db")
      val db_test_as_list = db_test_file.getLines().filter(nonMLNElements(_)).map(MLNParser.parse(mln_exp, _)).foreach(x => println("parsed db: " + x))
      db_test_file.close()

    }

  }


}
