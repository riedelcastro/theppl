package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{Variable, State}
import com.github.riedelcastro.theppl.util._


/**
 * A term evaluates to a value given a possible world/state.
 * @author sriedel
 */
trait Term[+V] {
  def eval(state: State): Option[V]
  def value(state: State) = eval(state).get
  def variables: Iterable[Variable[Any]]
  def ===(that: Term[Any]) = Eq(this, that)
  def substitute(substitution: Substitution): Term[V] = this
  def ground: Term[V] = this
  def isConstant = variables.isEmpty

  /**
   * Iterates over all states for the hidden variables of this model.
   */
  def allStates = {
    val variables = this.variables.toSeq
    val domains = variables.map(_.domain).toSeq
    val tuples = StreamUtil.allTuples(domains)
    val states = tuples.map(State(variables, _))
    states
  }


}

trait Substitution {
  def get[T](variable: Variable[T]): Option[Term[T]]
  def variables:Set[Variable[Any]]
  def subset(vars:Set[Variable[Any]]):Substitution
}

object Substitution {
  def apply(map: Map[Variable[Any], Term[Any]]):Substitution = new Substitution {
    def get[T](variable: Variable[T]) = map.get(variable).map(_.asInstanceOf[Term[T]])
    def variables = map.keySet
    def subset(vars: Set[Variable[Any]]) = apply(map.filter(e => vars(e._1)))
  }
  /**
   * Iterates over all states for the given variables
   */
  def allGroundings(variables:Seq[Variable[Any]]) = {
    val domains = variables.map(_.domain).toSeq
    val tuples = StreamUtil.allTuples(domains)
    val substitutions = tuples.map(Substitution(variables, _))
    substitutions
  }

  def apply(variables: Seq[Variable[Any]], tuple: Seq[Any]): Substitution = {
    val map = variables.indices.map(i => variables(i) -> Constant(tuple(i))).toMap
    apply(map)
  }


}

case class Constant[+V](value: V) extends Term[V] {
  def eval(state: State) = Some(value)
  def variables = Seq.empty
  override def toString = value.toString
  override def isConstant = true
}

trait Composite[T, This <: Composite[T, This]] extends Term[T] {
  def parts: Seq[Term[Any]]
  def variables = parts.flatMap(_.variables).toSet.toSeq
  def genericCreate(p: Seq[Term[Any]]): This
  def genericEval(p: Seq[Any]): T
  def eval(state: State) = {
    val partsEval = parts.map(_.eval(state))
    if (partsEval.exists(_.isEmpty)) None else Some(genericEval(partsEval.map(_.get)))
  }

  override def ground = {
    genericCreate(parts.map(_.ground))
  }
  override def substitute(substitution: Substitution) = {
    genericCreate(parts.map(_.substitute(substitution)))
  }

  object Caster {
    implicit def cast[T](t: Any) = t.asInstanceOf[T]
  }


}

trait FunApp[R, F, This <: FunApp[R, F, This]] extends Composite[R, This] {
  def f: Term[Any]
  def args: Seq[Term[Any]]
  def parts = f +: args
  override def toString = f + args.mkString("(", ",", ")")

  def genericFunAppEval(f: F, args: Seq[Any]): R
  def genericEval(p: Seq[Any]) = genericFunAppEval(p(0).asInstanceOf[F], p.drop(1))

}

case class FunApp1[A1, R](f: Term[A1 => R],
                          a1: Term[A1])
  extends FunApp[R, A1 => R, FunApp1[A1, R]] {

  import Caster._

  def args = Seq(a1)
  def genericCreate(p: Seq[Term[Any]]) = FunApp1(p(0), p(1))
  def genericFunAppEval(f: A1 => R, args: Seq[Any]) = f(args(0))

}

case class FunApp2[A1, A2, R](f: Term[(A1, A2) => R],
                              a1: Term[A1], a2: Term[A2])
  extends FunApp[R, (A1, A2) => R, FunApp2[A1, A2, R]] {

  import Caster._

  def args = Seq(a1, a2)
  def genericCreate(p: Seq[Term[Any]]) = FunApp2(p(0), p(1), p(2))
  def genericFunAppEval(f: (A1, A2) => R, args: Seq[Any]) = f(args(0), args(1))
  override def toString = f match {
    case in: InfixFun[_, _, _] => "%s %s %s".format(a1, in.symbol, a2)
    case _ => super.toString
  }
}


class UniqueVar[+T](name: String, val domain: Seq[T]) extends Variable[T] with Term[T] {
  override def toString = name
}

case class Dom[+T](name: Symbol, values: Seq[T]) extends Builder1[UniqueVar[T], Nothing] {
  private var count = 0
  def newName() = {count += 1; "x" + count}
  def argument = new UniqueVar[T](newName(), values)
  def built = sys.error("empty")
  override def toString = name.toString()

  def -->[R](range: Dom[R]) = (this, range)

}

trait FunTerm1[A1, R] extends Term[A1 => R] {
  def apply(a: Term[A1]): FunApp1[A1, R] = FunApp1(this, a)
}
trait FunTerm2[A1, A2, R] extends Term[(A1, A2) => R] {
  def apply(a1: Term[A1], a2: Term[A2]): FunApp2[A1, A2, R] = FunApp2(this, a1, a2)
}


trait InfixFun[A1, A2, R] extends FunTerm2[A1, A2, R] {
  def symbol: String
}

object And extends Constant((x: Boolean, y: Boolean) => x && y) with InfixFun[Boolean, Boolean, Boolean] {
  def symbol = "&&"
}
object AndN extends Constant((args:Seq[Boolean]) => args.forall(identity(_))) {
  override def toString = "and"
}

object OrN extends Constant((args:Seq[Boolean]) => args.exists(identity(_)))

object Implies extends Constant((x: Boolean, y: Boolean) => !x || y) with InfixFun[Boolean, Boolean, Boolean] {
  def symbol = "==>"
}

object Eq extends Constant((a1: Any, a2: Any) => a1 == a2) with InfixFun[Any, Any, Boolean] {
  def symbol = "==="
}
object IntAdd extends Constant((x: Int, y: Int) => x + y) with InfixFun[Int, Int, Int] {
  def symbol = "+"
}

object DoubleAddN extends Constant((args:Seq[Double])=>args.sum)

object $ extends Constant((x: Boolean) => if (x) 1.0 else 0.0) with FunTerm1[Boolean, Double] {
  override def toString = "$"
}

case class TupleTerm2[T1, T2](arg1: Term[T1], arg2: Term[T2]) extends Composite[(T1, T2), TupleTerm2[T1, T2]] {

  import Caster._

  def parts = Seq(arg1, arg2)
  def genericCreate(p: Seq[Term[Any]]) = TupleTerm2(p(0), p(1))
  def genericEval(p: Seq[Any]) = (p(0), p(1))

}

case class SeqTerm[T](args:Seq[Term[T]]) extends Composite[Seq[T], SeqTerm[T]] {
  def parts = args
  def genericCreate(p: Seq[Term[Any]]) = SeqTerm(p.map(_.asInstanceOf[Term[T]]))
  def genericEval(p: Seq[Any]) = p.map(_.asInstanceOf[T])
  override def toString = args.mkString("[",",","]")
}

object Bool extends Dom('bools, Seq(false, true))


object LogicPlayground {


  //  case class FunApp2[A1,A2, R](f:Term[(A1,A2)=>R],a1:Term[A1],a2:Term[A2]) extends Term[R] {
  //    def eval(state: State) = f.eval(state)(a1.value(state),a2.value(state))
  //    def variables = (f.variables ++ a1.variables ++ a2.variables).toSet.toSeq
  //  }
  //  case class FunApp3[A1,A2,A3, R](f:Term[(A1,A2,A3)=>R],a1:Term[A1],a2:Term[A2],a3:Term[A3]) extends Term[R] {
  //    def eval(state: State) = f.eval(state)(a1.value(state),a2.value(state),a3.value(state))
  //    def variables = (f.variables ++ a1.variables ++ a2.variables ++ a3.variables).toSet.toSeq
  //  }

  trait GroundAtom[R] extends Variable[R] {
    def range: Dom[R]
    def domain = range.values
    def name:Symbol
    def args:Seq[Any]
    override def toString =  "'" + name + args.mkString("(",",",")")
  }

  case class GroundAtom1[A1, R](name: Symbol, a1: A1, range: Dom[R]) extends GroundAtom[R] {
    def args = Seq(a1)
  }
  case class GroundAtom2[A1, A2, R](name: Symbol, a1: A1, a2: A2, range: Dom[R]) extends GroundAtom[R]{
    def args = Seq(a1,a2)
  }


  trait Pred[F, R] extends Term[F] {
    def genericMapping(args: Seq[Any]): Variable[R]
    def name: Symbol
    def domains: Seq[Dom[Any]]
    def variables = StreamUtil.allTuples(domains.map(_.values)).map(genericMapping(_))
    override def toString = name.toString()
    def c[T](t: Any) = t.asInstanceOf[T]
  }
  
  object Pred {
//    def unapply[R](pred:Pred1[_, R]) = Some((pred.dom1,(a1:Any) => pred.mapping(a1.asInstanceOf[Any])))
  }
  
  case class Pred1[A1, R](name: Symbol, dom1: Dom[A1], range: Dom[R] = Bool)
    extends FunTerm1[A1, R] with Pred[A1 => R, R] {

    def domains = Seq(dom1)
    def genericMapping(args: Seq[Any]) = mapping(c[A1](args(0)))
    def mapping(a1: A1):GroundAtom1[A1,R] = GroundAtom1(name, a1, range)
    def apply(a1: A1) = mapping(a1)
    def eval(state: State) = Some((a1: A1) => state(mapping(a1)))
  }

  case class Pred2[A1, A2, R](name: Symbol, dom1: Dom[A1], dom2: Dom[A2], range: Dom[R] = Bool)
    extends FunTerm2[A1, A2, R] with Pred[(A1, A2) => R, R] {

    def domains = Seq(dom1, dom2)
    def genericMapping(args: Seq[Any]) = mapping(c[A1](args(0)), c[A2](args(1)))
    def mapping(a1: A1, a2: A2) = GroundAtom2(name, a1, a2, range)
    def apply(a1: A1, a2: A2) = mapping(a1, a2)
    def eval(state: State) = Some((a1: A1, a2: A2) => state(mapping(a1, a2)))
  }

  trait Quantification[T,R, This<:Quantification[T, R, This]] extends Term[R] {
    this:This =>

    def arguments:Seq[Variable[Any]]
    def term:Term[T]
    def create(vars:Seq[Variable[Any]],newTerm:Term[T]): This
    def aggregator:Term[Seq[T]=>R]
    def eval(state:State) = {
      val parts = State.allStates(variables).map(term.eval(_))
      val aggregator = this.aggregator.eval(state)
      if (aggregator.isEmpty || parts.exists(_.isEmpty)) None else Some(aggregator.get(parts.map(_.get)))
    }
    def variables = (term.variables.toSet -- variables).toSeq
    override def substitute(substitution: Substitution) = {
      //todo: need to consider that substitutions have free variables
      val fresh = (substitution.variables -- arguments)
      val subset = substitution.subset(fresh)
      create(arguments, term.substitute(subset))
    }
    override def ground = {
      val parts = Substitution.allGroundings(arguments).map(term.substitute(_).ground)
      FunApp1(aggregator,SeqTerm(parts))
    }
  }
  
  case class Forall(arguments: Seq[Variable[Any]], term: Term[Boolean])
    extends Quantification[Boolean,Boolean, Forall] {
    def create(vars: Seq[Variable[Any]], newTerm: Term[Boolean]) = Forall(vars,newTerm)
    def aggregator = AndN
  }
  case class Exists(arguments: Seq[Variable[Any]], term: Term[Boolean])
    extends Quantification[Boolean,Boolean, Exists] {
    def create(vars: Seq[Variable[Any]], newTerm: Term[Boolean]) = Exists(vars,newTerm)
    def aggregator = OrN
  }
  case class Sum(arguments: Seq[Variable[Any]], term: Term[Double])
    extends Quantification[Double,Double, Sum] {
    def create(vars: Seq[Variable[Any]], newTerm: Term[Double]) = Sum(vars,newTerm)
    def aggregator = DoubleAddN
  }


  def forall(builder: BuilderN[Variable[Any], Term[Boolean]]) = {
    Forall(builder.arguments, builder.built)
  }

  def sum(builder: BuilderN[Variable[Any], Term[Double]]) = Sum(builder.arguments, builder.built)

  case class Dot(variables: Seq[Variable[Any]], term: Term[Double]) extends Term[Double] {
    //todo: evaluate for each state to get a real vector, then dot product with weights
    //todo: feat = (value1,value2,..)
    def eval(state: State) = null
  }

  def dot(builder: BuilderN[Variable[Any], Term[Double]]) = {
    null
  }

  def reduce[T](term:Term[T]):Term[T] = {
    term match {
      case FunApp1(pred@Pred1(_,_,_),Constant(a1)) => pred.mapping(a1)
      case FunApp2(pred@Pred2(_,_,_,_),Constant(a1),Constant(a2)) => pred.mapping(a1,a2)
      case c:Composite[_,_] => 
        val parts = c.parts.map(reduce(_))
        val constants = parts.collect({case Constant(x) => x})
        if (constants.size == parts.size) Constant(c.genericEval(constants)) else c.genericCreate(parts)
      case _ => term
    }
  }
  
  //  implicit def toTT2[T1, T2](pair: (Term[T1], Term[T2])) = TupleTerm2(pair._1, pair._2)

  implicit def symbolToPredBuilder(name: Symbol) = {
    new AnyRef {
      def :=[T, R](doms: (Dom[T], Dom[R])) = Pred1(name, doms._1, doms._2)
      def :=[A1, A2, R](doms: (Dom[A1], Dom[A2], Dom[R])) = Pred2(name, doms._1, doms._2, doms._3)
    }
  }

  implicit def dom2ToTuple3[D1, D2](dom2: Tuple2[Dom[D1], Dom[D2]]) = new AnyRef {
    def ->[R](range: Dom[R]) = (dom2._1, dom2._2, range)
  }
  implicit def dom3ToTuple3[D1, D2, D3](dom: Tuple3[Dom[D1], Dom[D2], Dom[D3]]) = new AnyRef {
    def ->[R](range: Dom[R]) = (dom._1, dom._2, dom._3, range)
  }

  trait BooleanTermBuilder {
    def arg1: Term[Boolean]
    def &&(arg2: Term[Boolean]) = And(arg1, arg2)
    def ==>(arg2: Term[Boolean]) = Implies(arg1, arg2)

  }

  trait IntTermBuilder {
    def arg1: Term[Int]
    def +(arg2: Term[Int]) = IntAdd(arg1, arg2)
  }


  implicit def boolTermToBuilder(term: Term[Boolean]): BooleanTermBuilder = new BooleanTermBuilder {
    def arg1 = term
  }

  implicit def intTermToBuilder(term: Term[Int]): IntTermBuilder = new IntTermBuilder {
    def arg1 = term
  }

  implicit def intToConstant(x: Int) = Constant(x)

  implicit def intToBuilder(x: Int) = new AnyRef {
    def +(that: Term[Int]) = IntAdd(Constant(x), that)
  }




  def main(args: Array[String]) {
    val Person = Dom('persons, Range(0, 3))
    val smokes = new Pred1('smokes, Person, Bool) {
      //can be any client-side defined variable that is reused in other modules
      override def mapping(a1: Int) = GroundAtom1(name, a1, Bool)
    }
    val cancer = 'cancer := Person -> Bool
    val friends = 'friends := (Person, Person) -> Bool
    val test3 = forall {for (x <- Person) yield smokes(x)}
    println(test3)
    println(forall {for (x <- Person) yield And(cancer(x), smokes(x))})
    println(forall {for (x <- Person) yield cancer(x) && smokes(x)})
    println(forall {for (x <- Person) yield forall {for (y <- Person) yield cancer(x) ==> smokes(y)}})
    println(forall {for (x <- Person; y <- Person) yield friends(x, y) && friends(y, x)})
    println(forall {for (x <- Person; y <- Person) yield friends(x, y) && friends(y, x)})
    println(forall {for (x <- Person; y <- Person) yield friends(x + 1, 1 + 1 + y) === friends(y, x)})
    println(forall {for (x <- Person; y <- Person) yield (x + y) === (y + x)})
    println(sum {for (x <- Person; y <- Person) yield $((x + y) === (y + x))})
    //dot { for (s <- Single) yield sum { for (x <- Person) yield smokes(x)}  } //s<-Single can be omitted
    //dot { for (x <- Person) yield sum { for (y <- person) yield friend(x,y) }
    //ppl { 'f1 := dot { for (...) yield ... },
    //      'f2 := det { forall(...) yield ... }, ... }
    //ppl.weights('f1,"word") = 1.0
    //what happens if friends(x) with x \notin dom(friends(x)) ?

    val state = State(Map(smokes(1) -> true, friends(0, 1) -> false))
    println(state(smokes(1)))
    println(state.closed(smokes(2)))
    val f = forall {for (x <- Person) yield forall {for (y <- Person) yield cancer(x) ==> smokes(y)}}
    val grounded = f.ground
    println(grounded)
    println(reduce(grounded))


  }
}
