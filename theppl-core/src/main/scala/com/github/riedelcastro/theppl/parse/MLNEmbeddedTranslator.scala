package com.github.riedelcastro.theppl.parse

import scala.collection.mutable.{ListBuffer, HashMap, Seq}

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.term.TermImplicits._

/**
 * Translates the parsing output into processing statements on the fly.
 *
 * The full syntax of the .mln and .db files can be found in the user's manual;
 * http://alchemy.cs.washington.edu/user-manual/manual.html
 */
class MLNEmbeddedTranslator {

  val domain = new HashMap[String, Dom[Any]]
  val atoms = new HashMap[Symbol, Pred[_, _]]
  val formulae = new ListBuffer[(Double, Term[Boolean])]()


  /*
  *  A .mln file consists of two basic parts: declarations and formulas.

    The declaration section must contain at least one predicate,
    while the formulas section contains 0 or more formulas.
    Optionally, one can enumerate the constants of each type used in the .mln and .db files;
    if there is no enumeration, the set of constants is implied from all constants present in both files.
  *
  * */
  def translateMLNFromFile(file: String) = {
    val mln_file = scala.io.Source.fromFile(file)
    val filtered: Iterator[String] = mln_file.getLines().filter(nonMLNElements(_))
    val expressions = filtered map (MLNParser.parse(MLNParser.expression, _))



    for (expr <- expressions) expr.get match {

      /*Types and constants can be declared in an .mln file
      Each declared type must have at least one constant.
      A constant is considered to be declared the first  time it is encountered in
              a type declaration,
              a formula,
              or a ground atom (in a.db file). */

      case MLNParser.IntegerTypeDefinition(typeName, from, end) => {
        domain(typeName) = Dom(Symbol(typeName), (from to end))
      }

      case MLNParser.ConstantTypeDefinition(name, constants) => {
        val constantsAsSymbols = constants.map(x => Symbol(x))
        domain(name) = Dom(Symbol(name), constantsAsSymbols)
      }
      //predicate: String, args: List[Term]

      //a unary predicate
      //    Smokes(person)
      //    Cancer(person)
      //      val cancer = 'cancer := Persons -> Bool
      //        val smokes = 'smokes := Persons -> Bool
      //a binary predicate
      //    Friends(person, person)
      //        val friends = 'friends := (Persons, Persons) -> Bool

      //Atom(Friends,List(VariableOrType(person), VariableOrType(person)))
      //Atom(Smokes,List(VariableOrType(person)))
      case MLNParser.Atom(predicate, args) => {
        val types = args map (x => domain.getOrElseUpdate(x.toString, defaultDomain(x.toString)))

        val predicateDeclaration:Pred[_,_] = types match {
          case List(domain1) => Symbol(predicate) := domain1 -> Bool // type com.github.riedelcastro.theppl.term.Pred1
          case List(domain1, domain2) => Symbol(predicate) := (domain1, domain2) -> Bool //type com.github.riedelcastro.theppl.term.Pred2
          case _ => throw new Error("We do not support the predicate -arity: " + types.size)
        }
        atoms(Symbol(predicate)) = predicateDeclaration
      }
      // Friends(x, y) => (Smokes(x) <=> Smokes(y))
      //Implies(Atom(Friends,List(x, y)),Equivalence(Atom(Smokes,List(x)),Atom(Smokes,List(y))))
      //Implies(Atom(Smokes,List(x)),Atom(Cancer,List(x)))
//      case MLNParser.WeightedFormula(weight, formula) => {
//        addFormula(weight, formula)
//      }
//      case formula: MLNParser.Formula => {
//        addFormula(0.0, formula)
//      }
      case _ => println(" more comming... " + expr.get.toString)
    }
  }

  def defaultDomain(name: String): Dom[AnyRef] = {
    Dom(Symbol(name), Seq[AnyRef]())
  }

//  private def addFormula(weight: Double, f: Formula) = {
//    formulae += Tuple2(weight, formula(f))
//  }
//
//  private def formula(f: Formula): Term[Boolean] = {
//    f match {
//      case MLNParser.Atom(predicate, args) => {
//        atoms(Symbol(predicate.toString))
//      }
//      case MLNParser.And(lhs, rhs) => formula(lhs) && formula(rhs)
//      case MLNParser.Implies(lhs, rhs) => formula(lhs) |=> formula(rhs)
//      case MLNParser.Equivalence(lhs, rhs) => formula(lhs) === formula(rhs)
//      case _ => throw new Error("more formulae in progress..")
//    }
//  }

  /*
    * A .db file consists of a

      set of ground atoms, one per line.
      Evidence predicates are assumed by default to be closed-world, meaning that if they are not present in the .db file, they are assumed false.(closed-world assumption: a ground atom not in the database is assumed to be false)
      Non-evidence predicates, on the other hand, are assumed open-world by default.

    * */
  def translateDatabaseFromFile(file: String) = {
    /*
  List(DatabaseAtom(Friends,List(Constant(Gary), Constant(Helen)),true))
 List(DatabaseAtom(Friends,List(Constant(Helen), Constant(Gary)),true))
 List(DatabaseAtom(Friends,List(Constant(Gary), Constant(Anna)),true))
 List(DatabaseAtom(Friends,List(Constant(Anna), Constant(Gary)),true))
 List(DatabaseAtom(Smokes,List(Constant(Anna)),true))
 List(DatabaseAtom(Smokes,List(Constant(Edward)),true))
 List(DatabaseAtom(Smokes,List(Constant(Frank)),true))
List(DatabaseAtom(Smokes,List(Constant(Gary)),true))
 List(DatabaseAtom(Cancer,List(Constant(Anna)),true))
 List(DatabaseAtom(Cancer,List(Constant(Edward)),true))
     */

  }

  def nonMLNElements(x: String): Boolean = {
    /*Methods with empty parameter lists are, by convention, evaluated for their side-effects.
     Methods without parameters are assumed to be side-effect free. That's the convention. */
    !((x startsWith "//") || (x isEmpty))
  }


}
