package com.github.riedelcastro.theppl.parse

import com.github.riedelcastro.theppl.term.{Pred, Loglinear, Index}
import com.github.riedelcastro.theppl.{State, Variables, VecVar}
import com.github.riedelcastro.theppl.learn.LinearLearner

/**
 * Created by larysa  05.09.13
 *
 * Markov Logic frontend API.
 * Method chaining style.
 */
class MarkovLogic {

  val MLN = new MLNEmbeddedTranslator

  def infer = new Infer

  def learnwts = new Learnwts

  /** Performs the basic task: Inference */
  class Infer {
    def input(in: String): this.type = {
      MLN.translateMLNFromFile(in)
      this
    }

    def result(out: String): this.type = {
      //todo: writing the result to the provided file.
      this
    }

    def evidence(db: String): this.type = {
      MLN.translateDatabaseFromFile(db)
      this
    }

    def query(predicate: Seq[String]): Unit = {
      val formulae = MLN.formulae2

      val index = new Index()
      val features = MLN.processFormulae(index, formulae)

      val weightsVar = VecVar('weights)
      val mln = Loglinear(features, weightsVar)

      val observedPredicates = Variables.AllAtoms(allPredicates)
      val thisWorld2 = MLN.state2
      val state = State(thisWorld2).closed(observedPredicates)

      val hidden = Variables.AllAtoms(predicatesByName(predicate))
      val trainingSet = Seq(state).map(_.hide(hidden))
      val learnedWeights = LinearLearner.learn(mln)(trainingSet)

//      todo: delete this
      val inverseIndex = index.inverse()
      println("----")
      println(learnedWeights.toMap.map({
        case (index, value) => inverseIndex.get(index).map(_.mkString(",")) -> value
      }).mkString("\n"))

    }

    private def allPredicates = MLN.predicates.values.map(x => x.asInstanceOf[Pred[_, _]]).toSet

    private def predicatesByName(names: Seq[String]): Set[Pred[_, _]] = {
      val filtered = MLN.predicates.retain((k, v) => names.contains(k.name))
      filtered.values.map(x => x.asInstanceOf[Pred[_, _]]).toSet
    }

  }

  class Learnwts {
    def input(in: String): this.type = {
      this
    }

    def output(out: String): this.type = {
      this
    }

    def train(db: String): this.type = {
      this
    }


    def nonEvidence(db: String): Unit = {
    }

  }

}
