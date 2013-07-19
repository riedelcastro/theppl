package com.github.riedelcastro.theppl.parse

import com.github.riedelcastro.theppl.State
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.Pred1
import com.github.riedelcastro.theppl.term.TermImplicits._

/**
 * Created by larysa  15.07.13
 *
 * POC markov logic example based on Alchemy input files.
 */
object MLN extends App {
  val mln_file = "theppl-core/src/test/data/mln/social-network/smoking.mln"
  val db_file = "theppl-core/src/test/data/mln/social-network/smoking-train.db"

  val MLN = new MLNEmbeddedTranslator
  MLN.translateMLNFromFile(mln_file)
  MLN.translateDatabaseFromFile(db_file)

  /** markov logic in action */
  val thisWorld = MLN.state

  val possibleWorld = State(thisWorld)


  //todo: how do we decide which predicate is hidden and which observed?
  println("state" + possibleWorld)

  val formulae = MLN.formulae

  println(formulae)
  val domain /*: Map[String, Dom[Any]] */ = MLN.domain

  println(domain)

  val index = new Index()

  for (tuple <- formulae) {
    val weight = tuple._1
    val term = tuple._2
    processFormula(term)
  }


  def processFormula(term: Term[_]) /*: QuantifiedVecSum =*/ {
    term match {
      case pred1: FunTerm1[Any, _] => {
        printf("Pred1: processing %s  \n", pred1)
        val p = pred1.asInstanceOf[Pred1[Any, _]]
        val domainName: String = p.dom1.name.name
        val currentDomain = domain(domainName)
        val f1 = vecSum {
          for (value <- currentDomain) yield index(p.name) --> I {
            pred1(value).asInstanceOf[Term[Boolean]]
          }
        }
        println("vecSum f1 " + f1)
        //        println(f1.eval(possibleWorld.closed(Variables.AllAtoms(Set(p)))).get)
        //        println(index)
      }

      case pred2: FunTerm2[Any, Any, _] => {
        printf("Pred2: processing %s  \n", pred2)
        val p = pred2.asInstanceOf[Pred2[Any, Any, _]]
        val domainName1: String = p.dom1.name.name
        val domainName2: String = p.dom2.name.name
        val domain1 = domain(domainName1)
        val domain2 = domain(domainName2)

        val f2 = vecSum {
          for (first <- domain1; second <- domain2) yield index(p.name) --> I {
            pred2(first, second).asInstanceOf[Term[Boolean]]
          }
        }
      }

      case funApp1: FunApp1[_, _] => {
        printf("FunApp1: processing %s \n", funApp1)
        val operator = funApp1.f
        val first = funApp1.a1

      }
      case funApp2: FunApp2[_, _, _] => {
        printf("FunApp2: processing %s\n ", funApp2)
        val operator = funApp2.f
        val first = funApp2.a1
        val second = funApp2.a2
        //todo: how would we compute the sufficient statistics for any generic formula? I mean, how could this be done recursively?
      }
      case _ => term
    }
  }


  //    Smokes(x) => Cancer(x)
  //  val f3 = vecSum {
  //    for (p <- Persons) yield index('smoking_is_bad) --> I {
  //      smokes(p) |=> cancer(p)
  //    }
  //  }

}
