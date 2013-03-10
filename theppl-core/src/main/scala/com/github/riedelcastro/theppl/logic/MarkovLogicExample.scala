package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.infer.Argmaxer
import com.github.riedelcastro.theppl.VectorVar


/**
 * @author Sebastian Riedel
 */
object MarkovLogicExample {

  def main(args: Array[String]) {
    import com.github.riedelcastro.theppl.logic.LogicImplicits._
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

    //build a world in which smoking implies cancer
    val state = State(Map(smokes('Anna) -> true, cancer('Anna) -> true, smokes('Peter) -> false, cancer('Peter) -> true))

    //a simple potential that returns 1 if 'Anna smokes, and 0 otherwise
    val pot1 = Iverson(smokes(Constant('Anna)))

    //the same potential created with implicits
    //caveat: not exactly the same potential, because smokes('Anna) is directly converted to a variable
    //not to the predicate applied to a Constant('Anna) as above
    val pot2 = I { smokes('Anna) }

    //let's test the potential
    println(pot2.eval(state)) //should be 1.0

    //let's find the potential's argmax world
    println(pot2.argmax().state) //should be smokes(Anna) -> true

    //let's do a first order term now
    val firstOrder = sum { for (p <- Persons) yield I(smokes(p) ==> cancer(p)) }

    //a weighted version
    val weighted = sum { for (p <- Persons) yield I(smokes(p) ==> cancer(p)) * -1.5 }

    //a feature vector with single
    val feats = vector { for (p <- Persons) yield 'f --> I { smokes(p) ==> cancer(p) } }

    //a bias
    val bias = vector { for (p <- Persons) yield 'bias --> I { smokes(p) } }

    println((feats + bias).eval(state))

    //a weight vector
    val w1 = ParameterVector.fromMap(Map(List('f) -> -1.0))

    //a linear model
    val m1 = feats dot w1

    //should be -2.0
    println(m1.eval(state))

    //weight variable to learn
    val w2 = VectorVar('w2)

    //parametrized model
    val m2 = feats dot w2

    //learn weights
    val m3 = ((feats + bias) dot w2) | w2 -> w1
    //val w3 = OnlineLearner.learn(m2)(Seq(state,state))

    val weights = VectorVar('weights)
    val model = (feats + bias) dot weights

    //training example
    val instance = State(Map(
      Target(smokes('Anna)) -> true,
      Target(cancer('Anna)) -> true,
      Target(smokes('Peter)) -> false,
      Target(cancer('Peter)) -> true))

    val learned = Learner.learn(model)(Seq(state))
    println("Learned:")
    println(learned)


    //state-dependent
    //val persons = SetVar[Symbol]("persons")
    //state = State(Map( persons -> Set('Anna,'Peter)), Target(smokes('Anna)) -> true )
    //val weighted = sum { for (p <- persons) yield $(smokes(p) ==> cancer(p)) * -1.5}

    //a feature function
    //val feat = vector { for (s <- Strings) yield ('feat,s) -> sum {for (p <- persons) yield $(name(p) === s)}}
    //val feat = sum { for (p <- persons) yield vector {name(p) -> 1.0}}
    //val feat = sum { for (p <- Persons) yield vector {('f,name(p)) -> $(smokes(p) ==> cancer(p))}
    //val feat = sum { for (p <- Persons) yield vector {'f -> $(smokes(p) ==> cancer(p))}
    //val weights = vector { 'f -> -1.5 }
    //val weights = VectorVar('weights)
    //val model = feat dot weights + baseMeasure
    //val potential = model.potential(state)
    //learner.train(model) { Seq(state1,state2) }


    //ground atom variables involved in formula
    println(firstOrder.term.variables)
    println(firstOrder.variables)
    println(State.allStates(firstOrder.arguments))

    //firstOrder formula should evaluate to 2.0
    println(firstOrder.eval(state))
    println(firstOrder.ground.eval(state))
    println(weighted.eval(state))


  }

}


case class Loglinear(features: Term[ParameterVector], weights: Variable[ParameterVector]) extends Potential {
  val self = Dot(features, weights)
  def hidden = self.variables
  def score(state: State) = self.eval(state).get
  override def substitute(substitution: Substitution) = self.substitute(substitution)
  override def ground = self.ground
}

object Learner {

  import LogicImplicits._

  def learn(model: Loglinear)(instances: Seq[State]): ParameterVector = {
    val weights = new ParameterVector()
    for (epochs <- 0 until 2) {
      for (instance <- instances) {
        println(model.substitute(Substitution(Seq(model.weights), Seq(weights))))
        val conditioned = (model | model.weights -> weights) | instance
        val argmaxer = Argmaxer(conditioned)
        val guess = argmaxer.argmax().state
        val gold = instance.target
        val guessFeats = model.features.eval(guess).get
        val goldFeats = model.features.eval(gold).get
        weights.add(goldFeats, 1.0)
        weights.add(guessFeats, -1.0)
      }
    }
    weights
  }
}