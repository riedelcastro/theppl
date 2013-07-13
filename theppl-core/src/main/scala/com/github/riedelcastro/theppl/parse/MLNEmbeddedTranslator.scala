package com.github.riedelcastro.theppl.parse

import scala.collection.mutable.{ListBuffer, HashMap, Seq}

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.parse.MLNParser.Formula
import com.github.riedelcastro.theppl.Variable

/**
 * Translates the parsing output into processing statements on the fly.
 *
 * The full syntax of the .mln and .db files can be found in the alchemy user's manual;
 * http://alchemy.cs.washington.edu/user-manual/manual.html
 */
class MLNEmbeddedTranslator {

  val domain = new HashMap[String, Dom[Any]]
  val atoms = new HashMap[Symbol, Term[Any]]
  val formulae = new ListBuffer[(Double, Term[Boolean])]()
  val state = new HashMap[Variable[Any], Any]
//  val lookupGroundings = new HashMap[Symbol, ListBuffer[Term[Boolean]]]


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

      //a unary/binary predicate
      case MLNParser.Atom(predicate, args) => {
        val types = args map (x => domain.getOrElseUpdate(x.toString, defaultDomain(x.toString)))

        val predicateDeclaration = types match {
          case List(domain1) => Symbol(predicate) := domain1 -> Bool // type Pred1
          case List(domain1, domain2) => Symbol(predicate) := (domain1, domain2) -> Bool //type Pred2
          case _ => throw new Error("We do not support the predicate -arity: " + types.size)
        }
        atoms(Symbol(predicate)) = predicateDeclaration
      }
      // Friends(x, y) => (Smokes(x) <=> Smokes(y))
      //Implies(Atom(Friends,List(x, y)),Equivalence(Atom(Smokes,List(x)),Atom(Smokes,List(y))))
      case MLNParser.WeightedFormula(weight, formula) => {
        addFormula(weight, formula)
      }
      case formula: MLNParser.Formula => {
        addFormula(0.0, formula)
      }
      case _ => println(" more in progress... " + expr.get.toString)
    }

    mln_file.close()
  }

  def defaultDomain(name: String): Dom[AnyRef] = {
    Dom(Symbol(name), Seq[AnyRef]())
  }

  private def addFormula(weight: Double, f: Formula) = {
    formulae += Tuple2(weight, formula(f))
  }

  private def formula(f: Formula): Term[Boolean] = {
    f match {
      case MLNParser.Atom(predicate, args) => {
        atoms(Symbol(predicate.toString)).asInstanceOf[Term[Boolean]]
      }
      case MLNParser.And(lhs, rhs) => formula(lhs) && formula(rhs)
      case MLNParser.Implies(lhs, rhs) => formula(lhs) |=> formula(rhs)
      case MLNParser.Equivalence(lhs, rhs) => formula(lhs) === formula(rhs)
      case MLNParser.Or(lhs, rhs) => throw new Error("OR in progress..")
      case MLNParser.Not(form) => throw new Error("NOT in progress..")
      case _ => throw new Error("more formulae in progress..")
    }
  }

  /*
    * A .db file consists of a set of ground atoms, one per line.
      Evidence predicates are assumed by default to be closed-world,
      meaning that if they are not present in the .db file, they are assumed false.
      (closed-world assumption: a ground atom not in the database is assumed to be false)
      Non-evidence predicates, on the other hand, are assumed open-world by default.
    * */
  def translateDatabaseFromFile(file: String) = {
    val db_file = scala.io.Source.fromFile(file)
    val filtered: Iterator[String] = db_file.getLines().filter(nonMLNElements(_))
    val expressions = filtered map (MLNParser.parse(MLNParser.db, _))


    for (expr <- expressions) expr.get match {
      //    DatabaseAtom(Friends,List(Constant(Anna), Constant(Gary)),true)
      //    DatabaseAtom(Smokes,List(Constant(Anna)),true)
      case MLNParser.DatabaseAtom(predicate, args, positive) => {
        val predicateDeclaration = atoms(Symbol(predicate))
        predicateDeclaration match {
          case Pred1(name, dom1, range) => {
            args.head match {
              case MLNParser.Constant(value) => {
                enhanceDomain(dom1, value)
                val groundAtom = predicateDeclaration.asInstanceOf[Pred1[Any, Any]].apply(Symbol(value)) -> positive
                state += groundAtom
              }
            }
          }
          case Pred2(name, dom1, dom2, range) => {
            (args.head, args.last) match {
              case (MLNParser.Constant(value1), MLNParser.Constant(value2)) => {
                enhanceDomain(dom1, value1)
                enhanceDomain(dom2, value2)
                val groundAtom = predicateDeclaration.asInstanceOf[Pred2[Any, Any, Any]].apply(Symbol(value1), Symbol(value2)) -> positive
                state += groundAtom
              }
            }
          }
        }
      }
      case MLNParser.DatabaseFunction(returnValue, name, values) => throw new Error("OR in progress..")
      case _ => println("Not a database element...")
    }

    db_file.close()
  }

  private def enhanceDomain(dom: Dom[Any], value: String) {
    val domainName: String = dom.name.name
    val initial: Dom[Any] = domain.getOrElseUpdate(domainName, defaultDomain(domainName))
    val extendedDomain = initial.values :+ Symbol(value)
    domain.update(domainName, Dom(dom.name, extendedDomain.distinct))
  }

  def nonMLNElements(x: String): Boolean = {
    !((x startsWith "//") || (x isEmpty))
  }


}
