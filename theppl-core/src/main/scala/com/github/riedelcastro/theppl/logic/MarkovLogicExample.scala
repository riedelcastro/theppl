package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.State


/**
 * @author Sebastian Riedel
 */
object MarkovLogicExample {

  def main(args: Array[String]) {
    import com.github.riedelcastro.theppl.logic.LogicImplicits._
    //a domain of objects, here using scala Symbols, but can be anything
    val Person = Dom('persons, Seq('Anna, 'Peter))

    //a unary predicate
    val cancer = 'cancer := Person -> Bool

    //an alternative way to define a unary predicate
    val smokes = new Pred1('smokes, Person, Bool) {
      //can be any client-side defined variable that is reused in other templates
      override def mapping(a1: Symbol) = GroundAtom1(name, a1, Bool)
    }

    //a binary predicate
    val friends = 'friends := (Person, Person) -> Bool

    //build a world in which smoking implies cancer
    val state = State(Map(smokes('Anna) -> true, cancer('Anna) -> true, smokes('Peter) -> false, cancer('Peter) -> true))

    //a simple potential that returns 1 if 'Anna smokes, and 0 otherwise
    val pot1 = Iverson(smokes(Constant('Anna)))

    //the same potential created with implicits
    val pot2 = $(smokes('Anna))

    //let's test the potential
    println(pot2.eval(state)) //should be 1.0

    //let's find the potential's argmax world
    println(pot2.argmax().state) //should be smokes(Anna) -> true

    //let's do a first order term now
    val firstOrder = sum { for (p <- Person) yield $(smokes(p) ==> cancer(p)) }

    //ground atom variables involved in formula
    println(firstOrder.term.variables)
    println(firstOrder.variables)
    println(State.allStates(firstOrder.arguments))

    //firstOrder formula should evaluate to 2.0
    println(firstOrder.eval(state))
    println(firstOrder.ground.eval(state))


  }

}
