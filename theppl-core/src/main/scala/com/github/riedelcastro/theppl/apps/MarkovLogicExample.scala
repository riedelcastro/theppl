package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl.term._
import com.github.riedelcastro.theppl.{Variables, VecVar, State}
import com.github.riedelcastro.theppl.term.GroundAtom1
import com.github.riedelcastro.theppl.term.Pred1
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.term.TermImplicits._
import com.github.riedelcastro.theppl.term.GroundAtom1
import com.github.riedelcastro.theppl.term.Constant
import com.github.riedelcastro.theppl.term.Pred1
import com.github.riedelcastro.theppl.term.Dom
import com.github.riedelcastro.theppl.learn.LinearLearner

/**
 * @author Sebastian Riedel
 */
object MarkovLogicExample {

  def main(args: Array[String]) {
    import com.github.riedelcastro.theppl.term.TermImplicits._
    //a domain of objects, here using scala Symbols, but can be anything
    val Persons = Dom('persons, Seq('Anna, 'Peter))

    //a unary predicate
    val cancer = 'cancer := Persons -> Bool

    //an alternative way to define a unary predicate
    val smokes = new Pred1('smokes, Persons, Bool) {
      //can be any client-side defined variable that is reused in other templates
      override def mapping(a1: Symbol) = GroundAtom1(name, a1, Bool)
    }

    //a binary predicate
    val friends = 'friends := (Persons, Persons) -> Bool

    //an attribute
    val name = 'name := Persons -> Strings

    val hidden = Variables.GroundAtoms(Set(cancer))
    val observed = Variables.GroundAtoms(Set(smokes,friends))


    //build a worlds in which smoking implies cancer ... (and use closed world assumption).
    val state1 = State(Map(
      smokes('Anna) -> true, cancer('Anna) -> true,
      smokes('Peter) -> true, cancer('Peter) -> true,
      friends('Anna, 'Peter) -> true)).closed(observed)
    val state2 = State(Map(
      smokes('Anna) -> true, cancer('Anna) -> true,
      smokes('Peter) -> false, cancer('Peter) -> false,
      friends('Anna, 'Peter) -> false)).closed(observed)

    println(state1)
    println("---")
    println(state1.hide(hidden))

    //this index maps feature indices to integers and vice versa
    val index = new Index()

    //create MLN sufficient statistics formulae
    //note that this is a sum of singleton vectors, one for each person.
    //the singleton vector has a component at `index('smoke_bias)` that is 1 iff smokes(p) is true.
    val f1 = vecSum { for (p <- Persons) yield index('smoke_bias) --> I { smokes(p) } }
    val f2 = vecSum { for (p <- Persons) yield index('cancer_bias) --> I { cancer(p) } }
    val f3 = vecSum { for (p <- Persons) yield index('smoking_is_bad) --> I { smokes(p) ==> cancer(p) } }
    val f4 = vecSum { for (p1 <- Persons; p2 <- Persons) yield index('peer_pressure) --> I { (smokes(p1) && friends(p1, p2)) ==> smokes(p2) } }

    //the beauty of defining suff. statistics that way is that this
    //makes the "constant" depending weights very easy to implement, and  transparent
    //this creates a different component for every person...
    val f5 = vecSum { for (p <- Persons) yield index('smoke_bias, p) --> I { smokes(p) } }

    //the variable corresponding to the weight vector
    val weightsVar = VecVar('weights)

    val features = f2 //f1 + f2 + f3 + f4

    println("Feature Values:")
    println(features.eval(state1).get)
    println(features.eval(state2).get)
    println(index)



    //the mln is simply the dot product of weights and the sum of all the sufficient statistics
    val mln = Loglinear(features, weightsVar)

    //an actual weight vector
    val weights = new DenseVec(10)

    //setting the weights manually todo: make this more compact
    weights(index.index(Array('smoke_bias))) = 2.5

    //the MLN with fixed weights
    val fixedMLN = mln | weightsVar -> weights

    //training set (we hide cancer to learn how to predict it).
    val trainingSet = Seq(state1,state2).map(_.hide(hidden))

    val learnedWeights = LinearLearner.learn(mln)(trainingSet)

    val inverseIndex = index.inverse()
    println(learnedWeights)
    println("----")
    println(learnedWeights.toMap.map({case (index,value) => inverseIndex.get(index).map(_.mkString(",")) -> value}).mkString("\n"))

    val unrolled = Unroller.unrollAndGroupLogLinear(mln)

    //FROM HERE ON LEGACY CODE
    //a simple potential that returns 1 if 'Anna smokes, and 0 otherwise
    val pot1 = Iverson(smokes(Constant('Anna)))

    //the same potential created with implicits
    //caveat: not exactly the same potential, because smokes('Anna) is directly converted to a variable
    //not to the predicate applied to a Constant('Anna) as above
    val pot2 = I { smokes('Anna) }

    //let's test the potential
//    println(pot2.eval(state1)) //should be 1.0

    //let's find the potential's argmax world
//    println(pot2.argmax().state) //should be smokes(Anna) -> true

    //let's do a first order term now
    val firstOrder = sum { for (p <- Persons) yield I(smokes(p) ==> cancer(p)) }

    //a weighted version
    val weighted = sum { for (p <- Persons) yield I(smokes(p) ==> cancer(p)) * -1.5 }

    //a feature vector with single
    //    val feats = vector {
    //      for (p <- Persons) yield
    //        'f --> I { smokes(p) ==> cancer(p) }
    //    }
    //
    //    //peer pressure
    //    val peer = vector {
    //      for (p1 <- Persons; p2 <- Persons) yield
    //        'peer --> I { smokes(p1) && friends(p1, p2) ==> smokes(p2) }
    //    }
    //
    //
    //    //a bias
    //    val bias = vector {
    //      for (p <- Persons) yield
    //        'bias --> I { smokes(p) }
    //    }
    //
    //    println((feats + bias).eval(state))
    //
    //    //a weight vector
    //    val w1 = ParameterVector.fromMap(Map(List('f) -> -1.0))
    //
    //    //a linear model
    //    val m1 = feats dot w1
    //
    //    //should be -2.0
    //    println(m1.eval(state))
    //
    //    //weight variable to learn
    //    val w2 = VectorVar('w2)
    //
    //    //parametrized model
    //    val m2 = feats dot w2
    //
    //    //learn weights
    //    val m3 = ((feats + bias) dot w2) | w2 -> w1
    //    //val w3 = OnlineLearner.learn(m2)(Seq(state,state))
    //
    //    val weights = VectorVar('weights)
    //    val model = (feats + bias) dot weights
    //
    //    //training example
    //    val instance = State(Map(
    //      Target(smokes('Anna)) -> true,
    //      Target(cancer('Anna)) -> true,
    //      Target(smokes('Peter)) -> false,
    //      Target(cancer('Peter)) -> true))
    //
    //    val learned = Learner.learn(model)(Seq(instance))
    //    println("Learned:")
    //    println(learned)


  }

}
