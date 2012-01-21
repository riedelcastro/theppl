package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{Variable, State}
import com.github.riedelcastro.theppl.util.{Builder1, BuilderN, Builder, EmptyBuilt}


/**
 * A term evaluates to a value given a possible world/state.
 * @author sriedel
 */
trait Term[+V] {
  def eval(state: State): Option[V]
  def value(state: State) = eval(state).get
  def variables: Iterable[Variable[Any]]
}

case class Constant[+V](value: V) extends Term[V] {
  def eval(state: State) = Some(value)
  def variables = Seq.empty
}

trait Composite[T] extends Term[T] {
  def parts: Seq[Term[Any]]
  def variables = parts.flatMap(_.variables).toSet.toSeq
}

trait FunApp[R] extends Composite[R] {
  def f: Term[Any]
  def args: Seq[Term[Any]]
  def parts = f +: args
  override def toString = f + args.mkString("(", ",", ")")
  def reduce[T](t: Term[T], v: Option[T]) = v.map(Constant(_)).getOrElse(t)

}

case class FunApp1[A1, R](f: Term[A1 => R], a1: Term[A1]) extends FunApp[R] {

  def args = Seq(a1)
  def eval(state: State) = for (_f <- f.eval(state);
                                _a1 <- a1.eval(state)) yield _f(_a1)
  def ground(state: State): Term[R] = (f.eval(state), a1.eval(state)) match {
    case (Some(_f), Some(_a1)) => Constant(_f(_a1))
    case (_f, _a1) => FunApp1(reduce(f, _f), reduce(a1, _a1))
  }

}

case class FunApp2[A1, A2, R](f: Term[(A1, A2) => R], a1: Term[A1], a2: Term[A2]) extends FunApp[R] {

  def args = Seq(a1, a2)
  def eval(state: State) = for (_f <- f.eval(state);
                                _a1 <- a1.eval(state);
                                _a2 <- a2.eval(state)) yield _f(_a1, _a2)
  def ground(state: State): Term[R] = (f.eval(state), a1.eval(state), a2.eval(state)) match {
    case (Some(_f), Some(_a1), Some(_a2)) => Constant(_f(_a1, _a2))
    case (_f, _a1, _a2) => FunApp2(reduce(f, _f), reduce(a1, _a1), reduce(a2, _a2))
  }
}


class UniqueVar[T](name: String, val domain:Seq[T]) extends Variable[T] with Term[T] {
  override def toString = name
}

case class Dom[T](name: Symbol, values: Seq[T]) extends Builder1[UniqueVar[T], Nothing] {
  private var count = 0
  def newName() = {count += 1; "x" + count}
  def argument = new UniqueVar[T](newName(),values)
  def built = sys.error("empty")
  override def toString = name.toString()

  def -->[R](range: Dom[R]) = (this, range)

}

case class TupleTerm2[T1, T2](arg1: Term[T1], arg2: Term[T2]) extends Term[(T1, T2)] {
  def eval(state: State) = for (_a1 <- arg1.eval(state); _a2 <- arg2.eval(state)) yield (_a1, _a2)
  def variables = (arg1.variables ++ arg2.variables).toSet
}

object LogicPlayground {

  trait FunTerm1[A1, R] extends Term[A1 => R] {
    def apply(a: Term[A1]): FunApp1[A1, R] = FunApp1(this, a)
  }
  trait FunTerm2[A1, A2, R] extends Term[(A1, A2) => R] {
    def apply(a1: Term[A1], a2: Term[A2]): FunApp2[A1, A2, R] = FunApp2(this, a1, a2)
  }


  //  case class FunApp2[A1,A2, R](f:Term[(A1,A2)=>R],a1:Term[A1],a2:Term[A2]) extends Term[R] {
  //    def eval(state: State) = f.eval(state)(a1.value(state),a2.value(state))
  //    def variables = (f.variables ++ a1.variables ++ a2.variables).toSet.toSeq
  //  }
  //  case class FunApp3[A1,A2,A3, R](f:Term[(A1,A2,A3)=>R],a1:Term[A1],a2:Term[A2],a3:Term[A3]) extends Term[R] {
  //    def eval(state: State) = f.eval(state)(a1.value(state),a2.value(state),a3.value(state))
  //    def variables = (f.variables ++ a1.variables ++ a2.variables ++ a3.variables).toSet.toSeq
  //  }

  case class GroundAtom1[A1, R](name: Symbol, a1: A1, domain:Seq[R]) extends Variable[R]
  case class Pred1[A1, R](name: Symbol, dom1: Dom[A1], range: Dom[R] = Dom('bools, Seq(true, false)))
    extends FunTerm1[A1, R] {
    def mapping(a1: A1) = GroundAtom1(name, a1, range.values)
    def eval(state: State) = Some((a1: A1) => state(mapping(a1)))
    def variables = dom1.values.map(mapping(_))
    override def toString = name.toString()
  }
  case class Pred2[A1, A2, R](dom1: Seq[A1], dom2: Seq[A2], range: Seq[R], mapping: (A1, A2) => Variable[R]) extends Term[(A1, A2) => R] {
    def eval(state: State) = Some((a1: A1, a2: A2) => state(mapping(a1, a2)))
    def variables = Seq.empty //todo
  }

  def simplify[T](term: Term[T]): Term[T] = {
    term match {
      case FunApp1(Constant(f), Constant(a)) => Constant(f(a))
      case FunApp1(pred: Pred1[_, _], Constant(a)) => pred.mapping(a)
      case _ => term
    }
  }


  //alternative: introduce FunApp1, FunApp2[A1,A2,R](t:FunTerm2[A1,A2,R],a1:A1,a2:A2)
  //object Pred { def apply[D1,D2](dom1,dom2): FunTerm2[D1,D2,Boolean] }
  //Predicate fun term evaluates to a function that takes (d1,d2) values, maps them to a variable, and then returns
  //the value associated with this variable
  //val friends = Pred2(person,person)
  //val pred3 = Pred3(person,person,person)
  //friends(x,y)

  //  def $[T](t: T): Constant[T] = Constant(t)
  //  def $[A1,A2,R](f: (A1,A2)=>R):Constant[(A1,A2)=>R] = new Constant(f) with Function2[Term[A1],Term[A2],FunApp[(A1, A2),R]]{
  //    def apply(arg1:Term[A1],arg2:Term[A2]) = FunApp(this,TupleTerm2(arg1,arg2))
  //  }
  def $(i: Int) = Constant(i)
  def $[A, R](f: A => R) = new Constant(f) {
    def apply(a: Term[A]): FunApp1[A, R] = FunApp1(this, a)
  }

  def $2[A1, A2, R](f: Tuple2[A1, A2] => R) = new Constant(f) {
    def apply(a: (Term[A1], Term[A2])): FunApp1[(A1, A2), R] = FunApp1[Tuple2[A1, A2], R](this.asInstanceOf[Term[Tuple2[A1, A2] => R]], TupleTerm2(a._1, a._2))
  }

  def $22[A1, A2, R](f: (A1, A2) => R) = new Constant(f) with FunTerm2[A1, A2, R]

  case class Forall(variables: Seq[Variable[Any]], term: Term[Boolean]) extends Term[Boolean] {
    //todo: generate all states
    def eval(state: State) = None
  }
  def forall(builder: BuilderN[Variable[Any], Term[Boolean]]) = {
    Forall(builder.arguments, builder.built)
  }

  //  implicit def toTT2[T1, T2](pair: (Term[T1], Term[T2])) = TupleTerm2(pair._1, pair._2)

  implicit def symbolToPredBuilder(name: Symbol) = {
    new AnyRef {
      def :=[T, R](doms: (Dom[T], Dom[R])) = Pred1(name, doms._1, doms._2)
    }
  }

  trait BooleanTermBuilder {
    def arg1: Term[Boolean]
    def &&(arg2: Term[Boolean]) = And(arg1, arg2)

  }

  implicit def boolTermToBuilder(term: Term[Boolean]): BooleanTermBuilder = new BooleanTermBuilder {
    def arg1 = term
  }

  object And extends Constant((x: Boolean, y: Boolean) => x && y) with FunTerm2[Boolean, Boolean, Boolean] {
    override def toString = "and"
  }

  object Bools extends Dom('bools, Seq(true, false))


  def main(args: Array[String]) {
    val values = Range(0, 10)
    val add = (x: (Int, Int)) => x._1 + x._2
    //    val formula = for (x <- Dom(values); y <- Dom(values)) yield FunApp($(add), (x, y))
    val bools = Dom('bools, Seq(false, true))
    val test = for (x <- bools; y <- bools) yield x
    println(test.built)
    println(test.arguments)
    val f = forall {for (x <- bools; y <- bools) yield x}
    //$(add)($(1),$(2))
    println(f)
    val lt3 = (x: Int) => x < 3
    val lt = (x: Int, y: Int) => x < y
    val dom = Dom('dom, values)
    val test2 = forall {for (x <- dom) yield $(lt3)(x)}
    println(test2)
    println(forall {for (x <- dom; y <- dom) yield $22(lt)(x, y)})
    val persons = Dom('persons, Range(0, 10))
    val smokes = Pred1('smokes, persons, bools)
    val cancer = 'cancer := persons -> bools
    val test3 = forall {for (x <- persons) yield smokes(x)}
    println(test3)
    println(forall {for (x <- persons) yield And(cancer(x), smokes(x))})
    println(forall {for (x <- persons) yield cancer(x) && smokes(x)})

  }
}
