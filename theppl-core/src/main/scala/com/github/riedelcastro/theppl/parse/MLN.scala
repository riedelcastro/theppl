package com.github.riedelcastro.theppl.parse

import com.github.riedelcastro.theppl.{Variables, VecVar, Variable, State}
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.Variables.AtomSet
import com.github.riedelcastro.theppl.util.SetUtil.Union
import org.riedelcastro.nurupo.BuilderN
import com.github.riedelcastro.theppl.learn.LinearLearner

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
  /*Get all formulae and evidence elements*/
  //  val formulae = MLN.formulae
  //  println("formulae = " + formulae)

  val formulae2 = MLN.formulae2
  println("formulae2 = " + formulae2)

  val index = new Index()
  val featureVec = formulae2.map(tuple => processFormula(tuple._2).asInstanceOf[Term[Vec]])
  val features = featureVec.reduceLeft(_ + _)
  println("features = " + features)


  /** ****************************************************************************************/

  //this index maps feature indices to integers and vice versa
  //the variable corresponding to the weight vector
  val weightsVar = VecVar('weights)

  execTimeOf {
    //the mln is simply the dot product of weights and the sum of all the sufficient statistics
    val mln = Loglinear(features, weightsVar)



    /** ****************************************************************************************/

    //todo: programmaticaly create a set of observed predicates
    val smokes = MLN.predicate("Smokes").get.asInstanceOf[Pred[_, _]]
    val friends = MLN.predicate("Friends").get.asInstanceOf[Pred[_, _]]
    val observed = Variables.AllAtoms(Set(smokes, friends))
    val thisWorld = MLN.state
    val state = State(thisWorld).closed(observed)
    println("state: " + state)

    val vec = features.eval(state).get
    println("vec = " + vec)
    println("index = " + index)

    /** ****************************************************************************************/
    //todo: hidden  variables will be passed through an API call.
    //training set (we hide cancer to learn how to predict it).
    val hidden = Variables.AllAtoms(Set(smokes))
    val trainingSet = Seq(state).map(_.hide(hidden))

    /** ****************************************************************************************/
    //todo: currently only the brute force argmaxer works, and it takes too long on these examples
    //todo: to test the learner, for now we should look at less hidden ground atoms.
    //val learnedWeights = LinearLearner.learn(mln)(trainingSet)
    //println("learnedWeights = " + learnedWeights)
  }

  private def flattenUnion[T](sets:Set[Set[T]]):Set[Set[T]] = sets.flatMap(_ match {
    case Union(inner) => flattenUnion(inner)
    case set => Set(set)
  })

  private def processFormula(term: Term[_]): QuantifiedVecSum = {
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


  def execTimeOf[A](f: => A) = {
    val start = System.nanoTime
    val result = f
    println("time: " + (System.nanoTime - start) / 1000 + " ms")
    result
  }


}
