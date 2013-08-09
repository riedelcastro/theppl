package com.github.riedelcastro.theppl.parse

import com.github.riedelcastro.theppl.{Variable, State}
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.Variables.AllAtoms
import com.github.riedelcastro.theppl.util.SetUtil.Union
import org.riedelcastro.nurupo.BuilderN

/**
 * Created by larysa  15.07.13
 *
 * POC markov logic example based on Alchemy input files.

//todo: Q:how do we decide which predicate is hidden and which observed?
//todo: A: create API which simulates Alchemy CLI, and allow settings via parameters.
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


  def processFormula(term: Term[_]): QuantifiedVecSum = {

    val variables = term.variables /*grounded predicates*/
    val filtered = variables match {
        case Union(sets) => Union(sets.filterNot(_.isInstanceOf[AllAtoms]))
        case _ => variables
      }

    /**/
    val builderN = new BuilderN[Variable[Any], Term[Vec]] {
      val arguments = filtered.toSeq
      val built = index(Symbol(term.toString)) --> I {
        term.asInstanceOf[Term[Boolean]]
      }
    }
    vecSum(builderN)
  }

}
