package com.github.riedelcastro.theppl.parse

import com.github.riedelcastro.theppl.{Variables, VecVar, State}
import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.learn.LinearLearner

/**
 * Created by larysa  15.07.13
 *
 * POC markov logic example based on Alchemy input files.

//todo: Q:how do we decide which predicate is hidden and which observed?
//todo: A: create API which simulates Alchemy CLI, and allow settings via parameters.
 */
object MLN extends App {
  execTimeOf {
    val mln_file = "theppl-core/src/test/data/mln/social-network/smoking.mln"
    val db_file = "theppl-core/src/test/data/mln/social-network/smoking-train.db"

    val MLN = new MLNEmbeddedTranslator
    MLN.translateMLNFromFile(mln_file)
    MLN.translateDatabaseFromFile(db_file)

    /** markov logic in action */
    /*Get all formulae and evidence elements*/

    val formulae = MLN.formulae2
    println("formulae2 = " + formulae)

    val index = new Index()
    val features = MLN.processFormulae(index, formulae)
    println("features = " + features)

    /** ****************************************************************************************/
    //this index maps feature indices to integers and vice versa
    //the variable corresponding to the weight vector
    val weightsVar = VecVar('weights)

    //the mln is simply the dot product of weights and the sum of all the sufficient statistics
    val mln = Loglinear(features, weightsVar)

    /** ****************************************************************************************/
    //todo: programmaticaly create a set of observed predicates
    val smokes = MLN.predicate("Smokes").get.asInstanceOf[Pred[_, _]]
    val friends = MLN.predicate("Friends").get.asInstanceOf[Pred[_, _]]
    val cancer = MLN.predicate("Cancer").get.asInstanceOf[Pred[_, _]]
    val observed = Variables.AllAtoms(Set(smokes, friends, cancer))
    val thisWorld2=MLN.state2
    val state = State(thisWorld2).closed(observed)
    println("state: " + state)

    val evalVec: Option[Vec] = features.eval(state)
    val vec = evalVec.getOrElse(Vec.zero)
    println("vec = " + vec)
    println("index = " + index.toVerboseString)

    /** ****************************************************************************************/
    //todo: hidden  variables will be passed through an API call.
    //training set (we hide cancer to learn how to predict it).
    val hidden = Variables.AllAtoms(Set(cancer))
    val trainingSet = Seq(state).map(_.hide(hidden))

    /** ****************************************************************************************/
    //todo: currently only the brute force argmaxer works, and it takes too long on these examples
    //todo: to test the learner, for now we should look at less hidden ground atoms.
    val learnedWeights = LinearLearner.learn(mln)(trainingSet)
    println("learnedWeights = " + learnedWeights)

    val inverseIndex = index.inverse()
    println("----")
    println(learnedWeights.toMap.map({
      case (index, value) => inverseIndex.get(index).map(_.mkString(",")) -> value
    }).mkString("\n"))
  }



  def execTimeOf[A](f: => A) = {
    val start = System.nanoTime
    val result = f
    printf("time: %.3f sec", (System.nanoTime - start) / 1000000000.0)
    result
  }


}
