package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.learn.LinearLearner
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.VecVar
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.term.Loglinear

/**
 * @author Sebastian Riedel
 *         A MLN is essentially a loglinear model that defines
(a) a multi-dimensional feature function (sometimes called sufficient statistics)
(b) a weight vector variable

This is reflected in the example. One benefit of this formulation
is that it explicitly separates what is given (formulae) from what is
learned (weights), and this makes learning easy.
 */
object MarkovLogicExample {

  def main(args: Array[String]) {

    //a domain of objects, here using scala Symbols, but can be anything
    //    val Persons = Dom('persons, Seq('Anna, 'Peter))
    val Persons = Dom('persons, Seq('Anna, 'Bob, 'Edward, 'Frank, 'Chris, 'Daniel, 'Gary, 'Helen))
    //a unary predicate
    //    Smokes(person)
    //    Cancer(person)
    val cancer = 'cancer := Persons -> Bool
    val smokes = 'smokes := Persons -> Bool

    //a binary predicate
    //    Friends(person, person)
    val friends = 'friends := (Persons, Persons) -> Bool


    val index = new Index()


    //create MLN sufficient statistics formulae
    //note that this is a sum of singleton vectors, one for each person.
    //the singleton vector has a component at `index('smoke_bias)` that is 1 iff smokes(p) is true.
    val f1 = vecSum {
      for (p <- Persons) yield index('smoke_bias) --> I {
        smokes(p)
      }
    }

    val f2 = vecSum {
      for (p <- Persons) yield index('cancer_bias) --> I {
        cancer(p)
      }
    }
    //    Smokes(x) => Cancer(x)
    val f3 = vecSum {
      for (p <- Persons) yield index('smoking_is_bad) --> I {
        smokes(p) |=> cancer(p)
      }
    }

    // People with friends who smoke, also smoke
    // and those with friends who don't smoke, don't smoke
    //    Friends(x, y) => (Smokes(x) <=> Smokes(y))
    val f4 = vecSum {
      for (p1 <- Persons; p2 <- Persons) yield index('peer_pressure) --> I {
        //        smokes(p1) && friends(p1, p2) |=> smokes(p2)
        friends(p1, p2) |=> (smokes(p1) === smokes(p2))
      }
    }
    println(" Friends(x, y) => (Smokes(x) <=> Smokes(y))  §§§ " + f4)


    //the beauty of defining suff. statistics that way is that this
    //makes the "constant" depending weights very easy to implement, and  transparent
    //this creates a different component for every person...
    val f5 = vecSum {
      for (p <- Persons) yield index('smoke_bias, p) --> I {
        smokes(p)
      }
    }

    //example of equivalence //friendship is reflexive
    val f6 = vecSum {
      for (p1 <- Persons; p2 <- Persons) yield index('reflexive) --> I {
        friends(p1, p2) === friends(p2, p1)
      }
    }

    //example of existential quantification
    val f7 = vecSum {
      for (p1 <- Persons) yield index('everybody_has_a_friend) --> I {
        exists {
          for (p2 <- Persons) yield friends(p1, p2)
        }
      }
    }



    val observed = Variables.AllAtoms(Set(smokes, friends, cancer))


    //build a worlds in which smoking implies cancer ... (and use closed world assumption on observed predicates).
    //effectively this means that even though only one friend atom is defined per state,
    //the other 3 atoms are defined implicitly to be mapped to the default value for the predicate range (false).
//    val state1 = State(Map(
//      smokes('Anna) -> true,
//      cancer('Anna) -> true,
//      smokes('Peter) -> true,
//      cancer('Peter) -> true,
//      friends('Anna, 'Peter) -> true)).closed(observed)
//
//    val state2 = State(Map(
//      smokes('Anna) -> true,
//      cancer('Anna) -> true,
//      smokes('Peter) -> false,
//      cancer('Peter) -> false,
//      friends('Anna, 'Peter) -> false)).closed(observed)

    val stateMLN = State(Map(
      friends('Anna, 'Bob) -> true,
      friends('Bob, 'Anna) -> true,
      friends('Anna, 'Edward) -> true,
      friends('Edward, 'Anna) -> true,
      friends('Anna, 'Frank) -> true,
      friends('Frank, 'Anna) -> true,
      friends('Bob, 'Chris) -> true,
      friends('Chris, 'Bob) -> true,
      friends('Chris, 'Daniel) -> true,
      friends('Daniel, 'Chris) -> true,
      friends('Edward, 'Frank) -> true,
      friends('Frank, 'Edward) -> true,
      friends('Gary, 'Helen) -> true,
      friends('Helen, 'Gary) -> true,
      friends('Gary, 'Anna) -> true,
      friends('Anna, 'Gary) -> true,
      smokes('Anna) -> true,
      smokes('Edward) -> true,
      smokes('Frank) -> true,
      smokes('Gary) -> true,
      cancer('Anna) -> true,
      cancer('Edward) -> true)).closed(observed)

    //this index maps feature indices to integers and vice versa
    //the variable corresponding to the weight vector
    val weightsVar = VecVar('weights)

    //        val features = f2
    val features = f1 + f2 + f3 + f4

    println("features all = " + features)

    println("Feature Values MLN:")
    val evalVec1: Vec = features.eval(stateMLN).get
    println(evalVec1)
    //    println("Feature Values 2:")
    //    println(features.eval(state2).get)
    println(index)

    //the mln is simply the dot product of weights and the sum of all the sufficient statistics
    val mln = Loglinear(features, weightsVar)

    //an actual weight vector
    val weights = new DenseVec(10)

    //setting the weights manually todo: make this more compact
    weights(index.index(Array('smoke_bias))) = 2.5

    //the MLN with fixed weights
    val fixedMLN = mln | weightsVar -> weights

    //sets of variables we can use to compactly define states using the `close` and `hide` methods of states.
    val hidden = Variables.AllAtoms(Set(cancer))

    //training set (we hide cancer to learn how to predict it).
    //    val trainingSet = Seq(state1, state2).map(_.hide(hidden))
    val trainingSet = Seq(stateMLN).map(_.hide(hidden))

    val learnedWeights = LinearLearner.learn(mln)(trainingSet)
    println(learnedWeights)

    val inverseIndex = index.inverse()
    println("----")
    println(learnedWeights.toMap.map({
      case (index, value) => inverseIndex.get(index).map(_.mkString(",")) -> value
    }).mkString("\n"))


  }

}
