package com.github.riedelcastro.theppl.parse

import scala.collection.mutable.{ArrayBuffer, Seq, ListBuffer, HashMap}

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.parse.MLNParser._
import com.github.riedelcastro.theppl.Variable
import scala.collection.immutable.{List, Map}
import com.github.riedelcastro.theppl.Variables.AtomSet
import org.riedelcastro.nurupo.BuilderN
import scala._


import com.github.riedelcastro.theppl.term.Term
import com.github.riedelcastro.theppl.term.Not
import com.github.riedelcastro.theppl.util.{FrontletsMongoCache, LoanResourceManager}

import org.riedelcastro.frontlets.{MongoFrontletCollection, Frontlet}
import java.nio.file.Paths
import com.github.riedelcastro.theppl.parse.MLNParser.Atom
import com.github.riedelcastro.theppl.term.FunApp1
import com.github.riedelcastro.theppl.term.Pred2
import scala.AnyRef
import com.github.riedelcastro.theppl.parse.MLNParser.AsteriskAtom
import com.github.riedelcastro.theppl.parse.MLNParser.And
import com.github.riedelcastro.theppl.term.Pred1
import com.github.riedelcastro.theppl.parse.MLNParser.Equivalence
import com.github.riedelcastro.theppl.parse.MLNParser.PlusVariable
import com.github.riedelcastro.theppl.parse.MLNParser.Or
import com.github.riedelcastro.theppl.parse.MLNParser.PlusAtom
import com.github.riedelcastro.theppl.term.Constant
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.term.QuantifiedVecSum
import com.github.riedelcastro.theppl.parse.MLNParser.ExclamationVariable
import com.github.riedelcastro.theppl.util.SetUtil.Union
import com.github.riedelcastro.theppl.parse.MLNParser.VariableOrType
import com.github.riedelcastro.theppl.term.FunApp2
import scala.Tuple2
import scala.Error
import com.github.riedelcastro.theppl.parse.MLNParser.Implies
import com.google.common.cache.LoadingCache


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
  private val mlnPlusFormulae = new ListBuffer[(Double, Formula)]()

  private val uniqueVarsDictionary = new HashMap[String, UniqueVar[Any]]
  private val typedUniqueVarsDictionary = new HashMap[String, UniqueVar[Any]]

  private val groundAtomsByPred = new HashMap[String, ArrayBuffer[Frontlet]]
  private val cacheContainer = new HashMap[String, LoadingCache[String, MongoFrontletCollection[Frontlet]]]
  var dbName: String = ""


  def state2: Map[Variable[Any], Any] = buildFullState.toMap


  def domain: Map[String, Dom[Any]] = dom.toMap

  def predicate(name: String): Option[Term[Any]] = predicates.get(Symbol(name))

  def formulae2: List[(Double, Term[Boolean])] = mlnFormulae.toList

  //todo: make this code beautiful.
  private def buildFullState = {
    val fullState = new HashMap[Variable[Any], Any]
    val predicateNames: collection.Set[Symbol] = predicates.keySet

    predicateNames foreach (predicate => {
      val frontlet = getFrontletConstructorByName(predicate)
      val frontletCollection = cacheContainer.get(frontlet.getClass.getSimpleName).get.get(predicate.name)
      val atoms = frontletCollection.iterator
      val state = atoms.map(x => {
        val positive = x.get("positive").get
        val args = frontlet match {
          case f1: GroundAtom1Db => Seq(Constant(x.get("arg1").get))
          case f2: GroundAtom2Db => Seq(Constant(x.get("arg1").get), Constant(x.get("arg2").get))
        }
        buildGroundAtom2(predicate.name, args) -> positive
      }).toMap
      fullState ++= state
    })

    fullState
  }


  private def getFrontletConstructorByName(predicate: Symbol): Frontlet = {
    val predDef: Option[Term[Any]] = predicates.get(predicate)
    predDef.get match {
      case pred1: Pred1[_, _] => new GroundAtom1Db
      case pred2: Pred2[_, _, _] => new GroundAtom2Db
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

  /**
   * Processing of the (Alchemy) MLN file.
   * A .mln file consists of two basic parts: declarations and formulas.
   * The declaration section must contain at least one predicate,
   * while the formulas section contains 0 or more formulas.
   * Optionally, one can enumerate the constants of each type used in the .mln and .db files;
   * if there is no enumeration, the set of constants is implied from all constants present in both files.
   * @param file a path to the .mln file
   */
  def translateMLNFromFile(file: String) = {
    LoanResourceManager.withFileIteratorForMLN(file) {
      line => {
        val parse = MLNParser.parse(MLNParser.expression, line)
        processMLNEntry(parse)
      }
    }
  }


  private def processMLNEntry(expr: MLNParser.ParseResult[MLNParser.Expression]) {
    expr.get match {

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
        //todo: !-predicate identification. (provided in the declaration part of mln)
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

      /*Hard formula processing:
       a formula is ``hard''   (i.e., worlds that violate it should have zero or negligible probability).
       all formulas must be preceded by a weight or terminated by a period (but not both). */
      case MLNParser.HardFormula(formula) => {
        addFormula(Double.PositiveInfinity, formula)
      }

      case MLNParser.AsteriskFormula(formula) => {
        val formulas: List[Formula] = expandAsterisk(formula)
        formulas foreach (f => addFormula(0.0, f))
      }

      case formula: MLNParser.Formula => addFormula(0.0, formula)
      case _ => println(" more in progress... " + expr.get.toString)
    }
  }

  /* When predicates in a formula are preceded by *, consider all possible ways in which * can be replaced by !
     * e.g *student(x) ^ *professor(x) is expanded into four formulas:
          student(x) ^ professor(x)
          !student(x) ^ professor(x)
          student(x) ^ !professor(x)
          !student(x) ^ !professor(x)    */
  private def expandAsterisk(formula: Formula): List[Formula] = {
    val expandedFormula = formula match {
      case AsteriskAtom(predicate, args) => Atom(predicate, args) :: MLNParser.Not(Atom(predicate, args)) :: Nil
      case Atom(predicate, args) => formula :: Nil
      case And(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield And(l, r)
      case Or(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield Or(l, r)
      case Implies(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield Implies(l, r)
      case Equivalence(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield Equivalence(l, r)
      case MLNParser.Not(f) => throw new Error("Negation is not allowed together with *-operator.")
      case _ => throw new Error("Unknown operator.")
    }
    expandedFormula
  }

  def defaultDomain(name: String): Dom[AnyRef] = {
    Dom(Symbol(name), Seq[AnyRef]())
  }

  private def addFormula(weight: Double, f: Formula) = {
    if (f.allPlusVariables.size != 0) mlnPlusFormulae += Tuple2(weight, f)
    else mlnFormulae += Tuple2(weight, formula(f))
  }

  private def formula(f: Formula): Term[Boolean] = {
    f match {
      case MLNParser.Atom(predicate, args) => {
        /*atoms are applications of predicates/functions to the arguments*/
        val atomByName: Term[Any] = atom(predicate)
        val pred: Term[Boolean] = atomByName.asInstanceOf[Term[Boolean]]
        val funApp = args match {

          case List(a1) => {
            val pred1: Pred1[Any, Any] = pred.asInstanceOf[Pred1[Any, Any]]
            val variable: Term[Any] = a1 match {
              case VariableOrType(name) => uniqueVarsDictionary.getOrElseUpdate(name, pred1.dom1.argument)
              case MLNParser.Constant(value) => Constant(value)
              case ExclamationVariable(name) => throw new UnsupportedOperationException("Exclamation variable processing is not supported for now..")
            }
            FunApp1(pred1, variable)
          }

          case List(a1, a2) => {
            val pred2: Pred2[Any, Any, Any] = pred.asInstanceOf[Pred2[Any, Any, Any]]
            val variable1: Term[Any] = {
              a1 match {
                case VariableOrType(name) => uniqueVarsDictionary.getOrElseUpdate(name, pred2.dom1.argument)
                case MLNParser.Constant(value) => Constant(value)
                case ExclamationVariable(name) => throw new UnsupportedOperationException("Exclamation variable processing is not supported for now..")
              }
            }
            val variable2: Term[Any] = {
              a2 match {
                case VariableOrType(name) => uniqueVarsDictionary.getOrElseUpdate(name, pred2.dom2.argument)
                case MLNParser.Constant(value) => Constant(value)
                case ExclamationVariable(name) => throw new UnsupportedOperationException("Exclamation variable processing is not supported for now..")
              }
            }
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
      case MLNParser.Or(lhs, rhs) => formula(lhs) || formula(rhs)
      case MLNParser.Not(form) => Not(formula(form))
      case _ => throw new Error("more formulae in progress..")
    }
  }

  def atom(predicate: String): Term[Any] = {
    atoms(Symbol(predicate.toString))
  }

  private def generateNameFrom(s: String): String = {
    Paths.get(s).toFile.getName.split("\\.db").head
  }

  /**
   * Processing of the (Alchemy) database file.
   * A .db file consists of a set of ground atoms, one per line.
   * Evidence predicates are assumed by default to be closed-world,
   * meaning that if they are not present in the .db file, they are assumed false.
   * (closed-world assumption: a ground atom not in the database is assumed to be false)
   * Non-evidence predicates, on the other hand, are assumed open-world by default.
   *
   * @param file path to a .db file
   */
  //todo: ACHTUNG! this method does a lot of things !
  def translateDatabaseFromFile(file: String) = {
    dbName = generateNameFrom(file)
    LoanResourceManager.withFileIteratorForMLN(file) {
      line => {
        val parse = MLNParser.parse(MLNParser.db, line)
        processDBEntry(parse)
      }
    }

    fullDomainFormulae

    populateCache


  }


  private def populateCache {
    val predicateNames = predicates.keySet
    predicateNames foreach (predicate => {
      val frontlet: Frontlet = getFrontletConstructorByName(predicate)
      val loadingCache: LoadingCache[String, MongoFrontletCollection[Frontlet]] =
        cacheContainer.getOrElse(frontlet.getClass.getSimpleName,
          FrontletsMongoCache.from(dbName, () => frontlet))

      val frontletCollection: MongoFrontletCollection[Frontlet] =
        loadingCache.get(predicate.name)
      frontletCollection ++= groundAtomsByPred.get(predicate.name).get.iterator

      cacheContainer.put(frontlet.getClass.getSimpleName, loadingCache)
    })
  }

  private def processDBEntry(expr: MLNParser.ParseResult[Any]) = {
    expr.get match {
      //    DatabaseAtom(Friends,List(Constant(Anna), Constant(Gary)),true)
      //    DatabaseAtom(Smokes,List(Constant(Anna)),true)
      case MLNParser.DatabaseAtom(predicate, args, positive) => {
        //        val coll = MongoDBCache.from(dbName).get(predicate)
        val atomsCollection = groundAtomsByPred.getOrElseUpdate(predicate, new ArrayBuffer)
        val predicateDeclaration = atoms(Symbol(predicate))
        predicateDeclaration match {
          case Pred1(name, dom1, range) => {
            args.head match {
              case MLNParser.Constant(value) => {
                enhanceDomain(dom1, value)
                //                val atoms1 = new MongoFrontletCollection(coll, () => new GroundAtom1Db)
                atomsCollection += new GroundAtom1Db().arg1(value).positive(positive)

              }
            }
          }
          case Pred2(name, dom1, dom2, range) => {
            (args.head, args.last) match {
              case (MLNParser.Constant(value1), MLNParser.Constant(value2)) => {
                enhanceDomain(dom1, value1)
                enhanceDomain(dom2, value2)
                //                val atoms2 = new MongoFrontletCollection(coll, () => new GroundAtom2Db)
                atomsCollection += new GroundAtom2Db().arg1(value1).arg2(value2).positive(positive)
              }
            }
          }
        }
      }
      case MLNParser.DatabaseFunction(returnValue, name, values) => throw new Error("DB function in progress..")
      case _ => println("Not a database element...")
    }
  }


  private def fullDomainFormulae = {
    mlnFormulae ++= expandPlusVars
    val fullFormulae: ListBuffer[(Double, Term[Boolean])] = mlnFormulae.map(x => (x._1, injectDomain(x._2)))
    mlnFormulae.clear()
    mlnFormulae ++= fullFormulae
  }

  private def injectDomain(formula: Term[Any]): Term[Boolean] = {
    val groundedFormula = formula match {
      case pred1@Pred1(name, dom, range) => pred1Builder(name, dom, range)
      case pred2@Pred2(name, dom1, dom2, range) => pred2Builder(name, dom2, dom1, range)
      case funApp1@FunApp1(f, a1) => {
        f match {
          case pred1: Pred1[_, _] => {
            val arg: Term[Any] = term1(pred1, a1)
            FunApp1(injectDomain(pred1).asInstanceOf[Term[Any => Boolean]], arg)
          }
          case _ => FunApp1(f, injectDomain(a1)) /*Not*/
        }
      }
      case funApp2@FunApp2(f, a1, a2) => {
        val innerFunApp2 = f match {
          case pred2: Pred2[_, _, _] => {
            val arg1: Term[Any] = term2(pred2, a1)
            val arg2: Term[Any] = term2(pred2, a2)
            FunApp2(injectDomain(pred2).asInstanceOf[Term[(Any, Any) => Boolean]], arg1, arg2)
          }
          case _ => FunApp2(f, injectDomain(a1), injectDomain(a2)) /*And, Implies, Eq*/
        }
        innerFunApp2
      }
      case _ => throw new Error("unknown term...")
    }
    groundedFormula.asInstanceOf[Term[Boolean]]
  }

  private def expandPlusVars: List[(Double, Term[Boolean])] = {
    /*we can expand plus variables if domains are known.*/
    val expandedPlusFormulae: List[(Double, Formula)] = mlnPlusFormulae.flatMap(x => {
      for (v <- expandPlusVariable(x._2)) yield (x._1, v)
    }).toList

    val plusFormulae = expandedPlusFormulae.map(x => (x._1, formula(x._2)))
    plusFormulae.toList
  }

  private def expandPlusVariable(formula: Formula): List[Formula] = {
    formula match {
      case PlusAtom(name, args) => {
        val predDef: Term[Any] = atoms.get(Symbol(name)).get
        val expanded = args match {
          case List(a1) => {
            val dom1Name = predDef.asInstanceOf[Pred1[_, _]].dom1.name.name
            val domainVals = dom.get(dom1Name).get.values
            val instantiatedPlusVar = domainVals.map(value => Atom(name, List(MLNParser.Constant(value.toString))).asInstanceOf[Formula])
            instantiatedPlusVar.toList
          }
          case List(a1, a2) => {
            val pred2Def = predDef.asInstanceOf[Pred2[_, _, _]]
            val firstArgValues: scala.collection.Seq[MLNParser.Term] = a1 match {
              case PlusVariable(x) => {
                val dom1name: String = pred2Def.dom1.name.name
                val dom1values = dom.get(dom1name).get.values
                dom1values.map(v => MLNParser.Constant(v.toString))
              }
              case _ => Seq(a1)
            }
            val secondArgValues: scala.collection.Seq[MLNParser.Term] = a2 match {
              case PlusVariable(x) => {
                val dom2name: String = pred2Def.dom2.name.name
                val dom2values = dom.get(dom2name).get.values
                dom2values.map(v => MLNParser.Constant(v.toString))
              }
              case _ => Seq(a2)
            }
            val reconstructedFormulas = for (arg1 <- firstArgValues; arg2 <- secondArgValues) yield {
              Atom(name, List(arg1, arg2)).asInstanceOf[Formula]
            }
            reconstructedFormulas.toList
          }
        }
        expanded
      }
      case Atom(predicate, args) => formula :: Nil
      case And(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs); r <- expandPlusVariable(rhs)) yield And(l, r)
      case Or(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs); r <- expandPlusVariable(rhs)) yield Or(l, r)
      case Implies(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs); r <- expandPlusVariable(rhs)) yield Implies(l, r)
      case Equivalence(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs); r <- expandPlusVariable(rhs)) yield Equivalence(l, r)
      case MLNParser.Not(f) => for (x <- expandPlusVariable(f)) yield MLNParser.Not(x)
      case _ => throw new Error("Unknown operator.")
    }
  }

  private def buildGroundAtom2(predName: String, args: Seq[Constant[Any]]): GroundAtom[Any] = {
    val predicateDeclaration: Term[Any] = predicate(predName).get
    predicateDeclaration match {
      case pred1@Pred1(name, dom1, range) => {
        args match {
          //todo: type check of the constant value!
          case Seq(Constant(value)) => pred1.apply(Symbol(value.toString))
          case _ => sys.error("predicate of arity 1 can not take more than one argument.")
        }
      }
      case pred2@Pred2(name, dom1, dom2, range) => {
        args match {
          case Seq(Constant(value1), Constant(value2)) => pred2.apply(Symbol(value1.toString), Symbol(value2.toString))
          case _ => sys.error("predicate of arity 2 can take only two arguments.")
        }
      }
    }
  }

  /*argument instantiation from the predicate with 2 parameters*/
  private def term2(pred2: Pred2[_, _, _], a: Term[Any]): Term[Any] = {
    a match {
      case arg: UniqueVar[Any] => createTypedUniqueVar(pred2.dom1.name.name, arg.toString)
      case Constant(value) => Constant(Symbol(value.toString))
    }
  }

  /*argument instantiation from the predicate with a single param*/
  private def term1(pred: Pred1[_, _], a: Term[Any]): Term[Any] = {
    a match {
      case arg1: UniqueVar[Any] => createTypedUniqueVar(pred.dom1.name.name, a.toString)
      case Constant(value) => Constant(Symbol(value.toString))
    }
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
}

/*Frontlets for GroundAtoms artifacts*/
class GroundAtom1Db extends Frontlet {
  val arg1 = StringSlot("arg1")
  val positive = BooleanSlot("positive")
}

class GroundAtom2Db extends Frontlet {
  val arg1 = StringSlot("arg1")
  val arg2 = StringSlot("arg2")
  val positive = BooleanSlot("positive")
}
