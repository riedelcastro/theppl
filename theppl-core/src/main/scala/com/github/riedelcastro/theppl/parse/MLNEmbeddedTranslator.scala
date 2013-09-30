package com.github.riedelcastro.theppl.parse

import scala.collection.mutable.{Seq, ListBuffer, HashMap}

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.parse.MLNParser.Formula
import com.github.riedelcastro.theppl.Variable
import scala.collection.immutable.{List, Map}
import com.github.riedelcastro.theppl.Variables.AtomSet
import org.riedelcastro.nurupo.BuilderN
import scala._
import scala.AnyRef
import com.github.riedelcastro.theppl.term.FunApp1
import com.github.riedelcastro.theppl.term.Pred2
import com.github.riedelcastro.theppl.term.Pred1
import com.github.riedelcastro.theppl.term.Constant
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.term.QuantifiedVecSum
import scala.Tuple3
import com.github.riedelcastro.theppl.util.SetUtil.Union
import com.github.riedelcastro.theppl.parse.MLNParser.VariableOrType
import com.github.riedelcastro.theppl.term.FunApp2
import scala.Tuple2

/**
 * Translates the parsing output into processing statements on the fly.
 *
 * The full syntax of the .mln and .db files can be found in the alchemy user's manual;
 * http://alchemy.cs.washington.edu/user-manual/manual.html
 */
class MLNEmbeddedTranslator {
  val atoms = new HashMap[Symbol, Term[Any]]
  val dom = new HashMap[String, Dom[Any]]
  val predicates = new HashMap[Symbol, Term[Any]] /*predicates with full domains*/

  private val mlnFormulae = new ListBuffer[(Double, Term[Boolean])]()
  private val uniqueVarsDictionary = new HashMap[String, UniqueVar[Any]]
  private val typedUniqueVarsDictionary = new HashMap[String, UniqueVar[Any]]
  private val databaseAtoms = new ListBuffer[(Term[Any], Seq[Constant[Any]], Any)]

  def state2: Map[Variable[Any], Any] = databaseAtoms.map(x => (buildGroundAtom(x._1, x._2) -> x._3)).toMap

  def domain: Map[String, Dom[Any]] = dom.toMap

  def predicate(name: String): Option[Term[Any]] = predicates.get(Symbol(name))

  def formulae2: List[(Double, Term[Boolean])] = fullDomainFormulae

  private def fullDomainFormulae = mlnFormulae.map(x => (x._1, injectDomain(x._2))).toList

  private def buildGroundAtom(predDeclaration: Term[Any], args: Seq[Constant[Any]]): GroundAtom[Any] = {
    predDeclaration match {
      case Pred1(name, dom1, range) => {
        val pred1: Pred1[Any, Any] = pred1Builder(name, dom1, range)
        args match {
          //todo: type check of the constant value!
          case Seq(Constant(value)) => pred1.apply(Symbol(value.toString))
          case _ => sys.error("predicate of arity 1 can not take more than one argument.")
        }
      }
      case Pred2(name, dom1, dom2, range) => {
        val pred2: Pred2[Any, Any, Any] = pred2Builder(name, dom1, dom2, range)
        args match {
          case Seq(Constant(value1), Constant(value2)) => pred2.apply(Symbol(value1.toString), Symbol(value2.toString))
          case _ => sys.error("predicate of arity 2 can take only two arguments.")
        }
      }
    }
  }

  //create MLN sufficient statistics formulae
  //note that this is a sum of singleton vectors, one for each person.
  //the singleton vector has a component at `index('smoke_bias)` that is 1 iff smokes(p) is true.
  def processFormulae(index: Index, formulae: List[(Double, Term[Boolean])]): Term[Vec] = {
    //todo: tuple._1 weights are not considered anymore!!!
    val featureVec = formulae2.map(tuple => processFormula(index, tuple._2).asInstanceOf[Term[Vec]])
    val features = featureVec.reduceLeft(_ + _)
    features
  }

  private def processFormula(index: Index, term: Term[_]): QuantifiedVecSum = {
    val variables = term.variables
    val filtered = variables match {
      case Union(sets) =>
        val flattened = flattenUnion(sets)
        Union(flattened.filterNot(_.isInstanceOf[AtomSet]))
      case _ => variables
    }

    /*monads: as computational builder*/
    val builderN = new BuilderN[Variable[Any], Term[Vec]] {
      val arguments = filtered.toSeq
      val built = index(Symbol(term.toString)) --> I {
        term.asInstanceOf[Term[Boolean]]
      }
    }
    vecSum(builderN)
  }

  private def flattenUnion[T](sets: Set[Set[T]]): Set[Set[T]] = sets.flatMap(_ match {
    case Union(inner) => flattenUnion(inner)
    case set => Set(set)
  })


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

    expressions foreach (expr => expr.get match {

      /*Types and constants can be declared in an .mln file
      Each declared type must have at least one constant.
      A constant is considered to be declared the first  time it is encountered in
              a type declaration,
              a formula,
              or a ground atom (in a.db file). */

      case MLNParser.IntegerTypeDefinition(typeName, from, end) => {
        dom(typeName) = Dom(Symbol(typeName), (from to end))
      }

      case MLNParser.ConstantTypeDefinition(name, constants) => {
        val constantsAsSymbols = constants.map(x => Symbol(x))
        dom(name) = Dom(Symbol(name), constantsAsSymbols)
      }

      //a unary/binary predicate declaration: might have a default domain,
      // witch will be injected after database atoms are processed.
      case MLNParser.Atom(predicate, args) => {
        val types = args map (x => dom.getOrElseUpdate(x.toString, defaultDomain(x.toString)))

        val predicateDeclaration = types match {
          case List(domain1) => {
            Symbol(predicate) := domain1 -> Bool
          }
          case List(domain1, domain2) => {
            Symbol(predicate) := (domain1, domain2) -> Bool
          }
          case _ => throw new Error("We do not support the predicate -arity: " + types.size)
        }
        atoms(Symbol(predicate)) = predicateDeclaration
      }
      // Friends(x, y) => (Smokes(x) <=> Smokes(y))
      //Implies(Atom(Friends,List(x, y)),Equivalence(Atom(Smokes,List(x)),Atom(Smokes,List(y))))
      //todo: formula consisting of a single predicate e.g. Smokes(x)
      //todo: workaround: add default weight for the single predicate, to indicate this as a formula
      case MLNParser.WeightedFormula(weight, formula) => addFormula(weight, formula)

      /*todo: hard formula processing:
      a formula is ``hard''   (i.e., worlds that violate it should have zero or negligible probability). */
      case MLNParser.HardFormula(formula) => {
        addFormula(Double.PositiveInfinity, formula)
      }

      case formula: MLNParser.Formula => addFormula(0.0, formula)
      case _ => println(" more in progress... " + expr.get.toString)
    })

    mln_file.close()
  }

  def defaultDomain(name: String): Dom[AnyRef] = {
    Dom(Symbol(name), Seq[AnyRef]())
  }

  private def addFormula(weight: Double, f: Formula) = {
    mlnFormulae += Tuple2(weight, formula(f))
  }

  // Friends(x, y) => (Smokes(x) <=> Smokes(y))
  //Implies(Atom(Friends,List(x, y)),Equivalence(Atom(Smokes,List(x)),Atom(Smokes,List(y))))
  private def formula(f: Formula): Term[Boolean] = {
    f match {
      case MLNParser.Atom(predicate, args) => {
        /*atoms are applications of predicates/functions to the arguments*/
        val atomByName: Term[Any] = atom(predicate)
        val pred: Term[Boolean] = atomByName.asInstanceOf[Term[Boolean]]
        val funApp = args match {
          case List(a1) => {
            val pred1: Pred1[Any, Any] = pred.asInstanceOf[Pred1[Any, Any]]
            val varName: String = a1.asInstanceOf[VariableOrType].name
            val variable: UniqueVar[Any] = uniqueVarsDictionary.getOrElseUpdate(varName, pred1.dom1.argument)
            FunApp1(pred1, variable)
          }
          case List(a1, a2) => {
            val pred2: Pred2[Any, Any, Any] = pred.asInstanceOf[Pred2[Any, Any, Any]]
            val varName1: String = a1.asInstanceOf[VariableOrType].name
            val variable1: UniqueVar[Any] = uniqueVarsDictionary.getOrElseUpdate(varName1, pred2.dom1.argument)

            val varName2: String = a2.asInstanceOf[VariableOrType].name
            val variable2: UniqueVar[Any] = uniqueVarsDictionary.getOrElseUpdate(varName2, pred2.dom2.argument)

            FunApp2(pred2, variable1, variable2)
          }
        }
        funApp.asInstanceOf[Term[Boolean]]
      }
      case MLNParser.And(lhs, rhs) => {
        formula(lhs) && formula(rhs)
      }
      case MLNParser.Implies(lhs, rhs) => {
        formula(lhs) |=> formula(rhs)
      }
      case MLNParser.Equivalence(lhs, rhs) => {
        formula(lhs) === formula(rhs)
      }
      case MLNParser.Or(lhs, rhs) => throw new Error("OR in progress..")
      case MLNParser.Not(form) => throw new Error("NOT in progress..")
      case _ => throw new Error("more formulae in progress..")
    }
  }

  def atom(predicate: String): Term[Any] = {
    atoms(Symbol(predicate.toString))
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

    expressions foreach (expr => expr.get match {
      //    DatabaseAtom(Friends,List(Constant(Anna), Constant(Gary)),true)
      //    DatabaseAtom(Smokes,List(Constant(Anna)),true)
      case MLNParser.DatabaseAtom(predicate, args, positive) => {
        val predicateDeclaration = atoms(Symbol(predicate))
        predicateDeclaration match {
          case Pred1(name, dom1, range) => {
            args.head match {
              case MLNParser.Constant(value) => {
                enhanceDomain(dom1, value)
                /*here we just collect the information about the database atoms
                in order to use this later when we know the full domain.*/
                databaseAtoms += Tuple3(predicateDeclaration, Seq(Constant(value)), positive)
              }
            }
          }
          case Pred2(name, dom1, dom2, range) => {
            (args.head, args.last) match {
              case (MLNParser.Constant(value1), MLNParser.Constant(value2)) => {
                enhanceDomain(dom1, value1)
                enhanceDomain(dom2, value2)
                databaseAtoms += Tuple3(predicateDeclaration, Seq(Constant(value1), Constant(value2)), positive)
              }
            }
          }
        }
      }
      case MLNParser.DatabaseFunction(returnValue, name, values) => throw new Error("DB function in progress..")
      case _ => println("Not a database element...")
    })

    db_file.close()

  }

  private def injectDomain(formula: Term[Any]): Term[Boolean] = {
    val groundedFormula = formula match {
      case pred1@Pred1(name, dom, range) => pred1Builder(name, dom, range)
      case pred2@Pred2(name, dom1, dom2, range) => pred2Builder(name, dom2, dom1, range)
      case funApp1@FunApp1(f, a1) => {
        val (name, innerFunApp1) = f match {
          case pred1: Pred1[_, _] => {
            val domName = pred1.dom1.name.name
            (domName, injectDomain(pred1))
          }
          case _ => throw new Error("unknown term...")
        }
        val uniqueVar: UniqueVar[Any] = createTypedUniqueVar(name, a1.toString)

        FunApp1(innerFunApp1.asInstanceOf[Term[Any => Boolean]], uniqueVar)
      }
      case funApp2@FunApp2(f, a1, a2) => {
        val innerFunApp2 = f match {
          case pred2: Pred2[_, _, _] => (a1, a2) match {
            case (arg1: UniqueVar[Any], arg2: UniqueVar[Any]) => {
              val uniqueVar1: UniqueVar[Any] = createTypedUniqueVar(pred2.dom1.name.name, arg1.toString)
              val uniqueVar2: UniqueVar[Any] = createTypedUniqueVar(pred2.dom2.name.name, arg2.toString)

              FunApp2(injectDomain(pred2).asInstanceOf[Term[(Any, Any) => Boolean]], uniqueVar1, uniqueVar2)
            }
          }
          case _ => FunApp2(f, injectDomain(a1), injectDomain(a2)) /*And, Implies, Eq*/
        }
        innerFunApp2
      }
      case _ => throw new Error("unknown term...")
    }
    groundedFormula.asInstanceOf[Term[Boolean]]
  }

  /*unless predicate contains the name, build*/
  def pred2Builder(name: Symbol, dom2: Dom[Any], dom1: Dom[Any], range: Dom[Any]): Pred2[Any, Any, Any] = {
    predicates.getOrElseUpdate(name, buildPred2(name, dom2, dom1, range)).asInstanceOf[Pred2[Any, Any, Any]]
  }

  def pred1Builder(name: Symbol, dom: Dom[Any], range: Dom[Any]): Pred1[Any, Any] = {
    predicates.getOrElseUpdate(name, buildPred1(name, dom, range)).asInstanceOf[Pred1[Any, Any]]
  }

  private def buildPred1(name: Symbol, dom: Dom[Any], range: Dom[Any]): Pred1[Any, Any] = {
    val domainName: String = dom.name.name
    val fullDom = domain(domainName)
    Pred1(name, fullDom, range)
  }

  private def buildPred2(name: Symbol, dom2: Dom[Any], dom1: Dom[Any], range: Dom[Any]): Pred2[Any, Any, Any] = {
    val firstDomain = dom1.name.name
    val firstFullDom = domain(firstDomain)
    val secondDomain = dom2.name.name
    val secondFullDom = domain(secondDomain)
    Pred2(name, firstFullDom, secondFullDom, range)
  }

  private def createTypedUniqueVar(domainName: String, name: String): UniqueVar[Any] = {
    val dom1: Dom[Any] = dom(domainName)
    typedUniqueVarsDictionary.getOrElseUpdate(name, new UniqueVar[Any](name, dom1.values))
  }

  /*deriving the full domain of the MLN*/
  private def enhanceDomain(thisDom: Dom[Any], value: String) {
    val domainName: String = thisDom.name.name
    val initial: Dom[Any] = dom.getOrElseUpdate(domainName, defaultDomain(domainName))
    val extendedDomain = initial.values :+ Symbol(value)
    dom.update(domainName, Dom(thisDom.name, extendedDomain.distinct))
  }

  def nonMLNElements(x: String): Boolean = {
    !((x startsWith "//") || (x isEmpty))
  }


}
