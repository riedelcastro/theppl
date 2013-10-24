package com.github.riedelcastro.theppl.parse

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import com.github.riedelcastro.theppl.parse.MLNParser._

/**
 * Tests parsing the existing MLNs to theppl.
 */
class MLNParserTest extends FunSpec with MustMatchers {

  describe("MLNParser") {
    it("just logs parsing") {

      val mln_exp = expression

      val mln_dir = "theppl-core/src/test/data/mln/social-network/"

      val mln_file = scala.io.Source.fromFile(mln_dir + "smoking.mln")
      def nonMLNElements(x: String): Boolean = {
        /*Methods with empty parameter lists are, by convention, evaluated for their side-effects.
         Methods without parameters are assumed to be side-effect free. That's the convention. */
        !((x startsWith "//") || (x isEmpty))
      }
      val filter: Iterator[String] = mln_file.getLines().filter(nonMLNElements(_))
      val mln_as_list = filter map (MLNParser.parse(mln_exp, _))
      mln_as_list foreach (x => println("parsed mln: " + x))
      mln_file.close()


      val db = MLNParser.db
      val db_train_file = scala.io.Source.fromFile(mln_dir + "smoking-train.db")
      val filtered_db: Iterator[String] = db_train_file.getLines().filter(nonMLNElements(_))
      val parsed_db = filtered_db map (MLNParser.parse(db, _))
      parsed_db foreach (x => println("parsed train db: " + x))
      db_train_file.close()

      val db_test_file = scala.io.Source.fromFile(mln_dir + "smoking-test.db")
      val db_test_as_list = db_test_file.getLines().filter(nonMLNElements(_)).map(MLNParser.parse(db, _)).foreach(x => println("parsed test db: " + x))
      db_test_file.close()

    }

    it("MLN syntax playground") {

      val mln_exp = expression

      val test = "10.0 Same(+hallo,!po) /* Hallo\nDu Igel */ ^ \n (Popel(du,igel)) => Same(du, nuss)"
      val parser = MLNParser.parse(mln_exp, test)
      // parser = [3.36] parsed: WeightedFormula(10.0,Implies(And(Atom(Same,List(PlusVariable(hallo), ExclamationType(po))),Atom(Popel,List(du, igel))),Atom(Same,List(du, nuss))))
      parser.get must be(WeightedFormula(10.0, Implies(And(PlusAtom("Same", List(PlusVariable("hallo"), ExclamationVariable("po"))), Atom("Popel", List(VariableOrType("du"), VariableOrType("igel")))), Atom("Same", List(VariableOrType("du"), VariableOrType("nuss"))))))
      // must be
      println("parser = " + parser)

      val include = "#include \"Blah.mln\""
      val parse_include = MLNParser.parse(mln_exp, include)
      parse_include.get must be(Include("\"Blah.mln\""))
      //      println("parse_include = " + parse_include)

      val f1 = "!Smokes(x) ^ !Cancer(x)"
      val parse1 = MLNParser.parse(mln_exp, f1)
      parse1.get must be(And(Not(Atom("Smokes", List(VariableOrType("x")))), Not(Atom("Cancer", List(VariableOrType("x"))))))
      //      println(f1 + " = " + parse1)

      val f2 = "!Cancer(x) v Smokes(y) ^ !Friends(x,y)"
      val parse2 = MLNParser.parse(mln_exp, f2)
      parse2.get must be(Or(Not(Atom("Cancer", List(VariableOrType("x")))), And(Atom("Smokes", List(VariableOrType("y"))), Not(Atom("Friends", List(VariableOrType("x"), VariableOrType("y")))))))
      //      println(f2 + " = " + parse2)


      /*
      *   todo:
Friends(x,y) => !(Cancer(x) ^ Smokes(y))=[1.41] parsed: Implies(Atom(Friends,List(x, y)),Not(And(Atom(Cancer,List(x)),Atom(Smokes,List(y)))))
!MentionType(x,PRN) ^ Head(x,+h) ^ InClust(x,+c)=[1.49]
parsed: And(And(Not(Atom(MentionType,List(x, Constant(PRN)))),Atom(Head,List(x, PlusVariable(h)))),Atom(InClust,List(x, PlusVariable(c))))

      *
      * */
      val f3 = "(Cancer(x) ^ Smokes(y)) => !Friends(x,y)"
      val parse3 = MLNParser.parse(mln_exp, f3)
      parse3.get must be(MLNParser.Implies(And(Atom("Cancer", List(VariableOrType("x"))), Atom("Smokes", List(VariableOrType("y")))), Not(Atom("Friends", List(VariableOrType("x"), VariableOrType("y"))))))
      println(f3 + " = " + parse3)

      val f4 = "Friends(x,y) v !Cancer(x) v Smokes(y)"
      val parse4 = MLNParser.parse(mln_exp, f4)
      parse4.get must be(Or(Or(Atom("Friends", List(VariableOrType("x"), VariableOrType("y"))), Not(Atom("Cancer", List(VariableOrType("x"))))), Atom("Smokes", List(VariableOrType("y")))))
      println(f4 + "=" + parse4)

      val f5 = "Cancer(x) v *Smokes(y)"
      val parse5 = MLNParser.parse(mln_exp, f5)
      parse5.get must be(AsteriskFormula(Or(Atom("Cancer", List(VariableOrType("x"))), AsteriskAtom("Smokes", List(VariableOrType("y"))))))
      println(f5 + "=" + parse5)

      val f6 = "!(Cancer(x) ^ Smokes(y))"
      val parse6 = MLNParser.parse(mln_exp, f6)
      parse6.get must be(Not(And(Atom("Cancer", List(VariableOrType("x"))), Atom("Smokes", List(VariableOrType("y"))))))
      println(f6 + "=" + parse6)

      val f7 = "!(Cancer(x) ^ Smokes(y)) => Friends(x,y)"
      val parse7 = MLNParser.parse(mln_exp, f7)
      parse7.get must be(Implies(Not(And(Atom("Cancer", List(VariableOrType("x"))), Atom("Smokes", List(VariableOrType("y"))))), Atom("Friends", List(VariableOrType("x"), VariableOrType("y")))))
      println(f7 + "=" + parse7)

      val f8 = "Friends(x,y) => !(Cancer(x) ^ Smokes(y))"
      val parse8 = MLNParser.parse(mln_exp, f8)
      parse8.get must be(Implies(Atom("Friends", List(VariableOrType("x"), VariableOrType("y"))), Not(And(Atom("Cancer", List(VariableOrType("x"))), Atom("Smokes", List(VariableOrType("y")))))))
      println(f8 + "=" + parse8)

      val f9 = "!MentionType(x,PRN) ^ Head(x,+h) ^ InClust(x,+c)"
      val parse9 = MLNParser.parse(mln_exp, f9)
      parse9.get must be(And(And(Not(Atom("MentionType", List(VariableOrType("x"), Constant("PRN")))), PlusAtom("Head", List(VariableOrType("x"), PlusVariable("h")))), PlusAtom("InClust", List(VariableOrType("x"), PlusVariable("c")))))

      println(f9 + "=" + parse9)

      val f10 = "Friends(x, Anna) ^ Smokes(Anna) => Smokes(x)"
      val parse10 = MLNParser.parse(mln_exp, f10)
      parse10.get must be(Implies(And(Atom("Friends", List(VariableOrType("x"), Constant("Anna"))), Atom("Smokes", List(Constant("Anna")))), Atom("Smokes", List(VariableOrType("x")))))
      println(f10 + " = " + parse10)
    }


  }


}
