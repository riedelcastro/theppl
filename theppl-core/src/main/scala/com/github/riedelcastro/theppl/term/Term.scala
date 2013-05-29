package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl._
import com.github.riedelcastro.theppl.util._
import org.riedelcastro.nurupo.{BuilderN, Builder1}
import scala.util.Random
import scala.Some
import scala.Tuple3
import scala.Tuple2


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
  def |(condition: State): Conditioned[V] = Conditioned(this, condition)
  def |[T](assignment: (Variable[T], T)): Conditioned[V] = this | State.singleton(assignment._1, assignment._2)
  def default: V

  /**
   * Iterates over all states for the variables of this term.
   */
  def allStates = {
    val variables = this.variables.toSeq
    val domains = variables.map(_.domain).toSeq
    val tuples = CollectionUtil.allTuples(domains)
    val states = tuples.map(State(variables, _))
    states
  }

}

case class Substituted[+V](term: Term[V], condition: Substitution) extends Composite[V, Substituted[V]] {
  val conditioned = term.substitute(condition)
  def parts = Seq(conditioned)
  def genericCreate(p: Seq[Term[Any]]) = Substituted(p(0).asInstanceOf[Term[V]], condition)
  def genericEval(p: Seq[Any]) = p(0).asInstanceOf[V]
  def default = term.default
}

case class Conditioned[+V](term: Term[V], condition: State) extends Term[V] {
  def eval(state: State) = term.eval(state + condition)
  def variables = {
    def conditionVars(vars:Set[Variable[Any]]):Set[Variable[Any]] = vars match {
      case SetUtil.Union(args) => SetUtil.Union(args.map(conditionVars))
      case pa: Variables.PartialAtoms => pa.conditioned(condition)
      case _ => vars
    }
    val mapped = conditionVars(term.variables.toSet)
    SetUtil.SetMinus(mapped, condition.variables)
  }
  def default = term.default
  override def substitute(substitution: Substitution) = Conditioned(term.substitute(substitution), condition)
  override def ground = Conditioned(term.ground, condition)
  override def toString = term.toString + " | (" + condition + ")"
}

/**
 * Mapping from variables to terms.
 */
trait Substitution {
  def get[T](variable: Variable[T]): Option[Term[T]]
  def variables: Set[Variable[Any]]
  def subset(vars: Set[Variable[Any]]): Substitution
}

object Substitution {
  def apply(map: Map[Variable[Any], Term[Any]]): Substitution = new Substitution {
    def get[T](variable: Variable[T]) = map.get(variable).map(_.asInstanceOf[Term[T]])
    def variables = map.keySet
    def subset(vars: Set[Variable[Any]]) = apply(map.filter(e => vars(e._1)))
  }

  def apply(state: State): Substitution = new Substitution {
    def get[T](variable: Variable[T]) = state.get(variable).map(Constant(_))
    def subset(vars: Set[Variable[Any]]) = apply(state - vars)
    def variables = state.variables
  }

  /**
   * Iterates over all states for the given variables
   */
  def allGroundings(variables: Seq[Variable[Any]]) = {
    val domains = variables.map(_.domain).toSeq
    val tuples = CollectionUtil.allTuples(domains)
    val substitutions = tuples.map(Substitution(variables, _))
    substitutions
  }

  def apply(variables: Seq[Variable[Any]], tuple: Seq[Any]): Substitution = {
    val map = variables.indices.map(i => variables(i) -> Constant(tuple(i))).toMap
    apply(map)
  }


}

/**
 * A constant evaluates to the same value in every world.
 * @param value the value the term evaluates to.
 * @tparam V the type of the value.
 */
case class Constant[+V](value: V) extends Term[V] {
  def eval(state: State) = Some(value)
  def variables = Seq.empty
  override def toString = value.toString()
  def default = value
}

object Caster {
  implicit def cast[T](t: Any) = t.asInstanceOf[T]
}

/**
 * A term that is composed of subterms.
 * @tparam T the type of value this term describes
 * @tparam This self type.
 */
trait Composite[+T, +This <: Composite[T, This]] extends Term[T] {
  def parts: Seq[Term[Any]]
  def variables: Iterable[Variable[Any]] = SetUtil.Union(parts.map(_.variables.toSet).toSet)
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

  //  override def |(condition: State) = genericCreate(parts.map(_ | condition))


}

trait FunApp[+R, F, This <: FunApp[R, F, This]] extends Composite[R, This] {
  def f: Term[Any]
  def args: Seq[Term[Any]]
  def parts = f +: args
  override def toString = f + args.mkString("(", ",", ")")

  def genericFunAppEval(f: F, args: Seq[Any]): R
  def genericEval(p: Seq[Any]) = genericFunAppEval(p(0).asInstanceOf[F], p.drop(1))
  override def variables = f match {
    case p: Pred[_, _] =>
      val union = SetUtil.Union(args.toSet.map((term: Term[Any]) => term.variables.toSet) + Variables.PartialAtoms(p, args))
      union
    case _ => super.variables
  }
}


case class FunApp1[A1, R](f: Term[A1 => R],
                          a1: Term[A1])
  extends FunApp[R, A1 => R, FunApp1[A1, R]] {

  import Caster._

  def args = Seq(a1)
  def genericCreate(p: Seq[Term[Any]]) = FunApp1(p(0), p(1))
  def genericFunAppEval(f: A1 => R, args: Seq[Any]) = f(args(0))
  def default = f.default(a1.default)
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
  def default = f.default(a1.default, a2.default)
}

object Applied {
  def unapply(term: Term[Any]): Option[(Term[Any], Seq[Term[Any]])] = {
    term match {
      case f: FunApp[_, _, _] => Some((f.f, f.args))
      case _ => None
    }
  }
}


object Applied1 {
  def unapply(term: Term[Any]): Option[(Term[Any], Term[Any])] = {
    term match {
      case f: FunApp1[_, _] => Some((f.f, f.a1))
      case _ => None
    }
  }
}

object Applied2 {
  def unapply(term: Term[Any]): Option[(Term[Any], Term[Any], Term[Any])] = {
    term match {
      case f: FunApp2[_, _, _] => Some((f.f, f.a1, f.a2))
      case _ => None
    }
  }
}

//object Applied2Typed {
//  def unapply[A,B](term:Term[Any]):Option[(Term[(A,B)=>Any], Term[A], Term[B])] = {
//    term match {
//      case f:FunApp2[_,_,_] => Some((f.f,f.a1,f.a2))
//      case _ => None
//    }
//  }
//}


class UniqueVar[+T](name: String, val domain: Seq[T]) extends Variable[T] with Term[T] {
  override def toString = name
}

case class Dom[+T](name: Symbol, values: Seq[T]) extends Builder1[UniqueVar[T], Nothing] {
  private var count = 0
  def newName() = { count += 1; "x" + count }
  def argument = new UniqueVar[T](newName(), values)
  def built = sys.error("empty")
  override def toString = name.toString()

  def -->[R](range: Dom[R]) = (this, range)

}

trait FunTerm1[A1, R] extends Term[A1 => R] {
  def apply(a: Term[A1]): FunApp1[A1, R] = FunApp1(this, a)
  def unapply(app: FunApp1[_, _]): Option[Term[A1]] = {
    if (app.f == this) Some(app.a1.asInstanceOf[Term[A1]]) else None
  }
}

trait FunTerm2[A1, A2, R] extends Term[(A1, A2) => R] {
  def apply(a1: Term[A1], a2: Term[A2]): FunApp2[A1, A2, R] = FunApp2(this, a1, a2)
  def unapply(app: FunApp2[_, _, _]): Option[(Term[A1], Term[A2])] = {
    if (app.f == this) Some((app.a1.asInstanceOf[Term[A1]], app.a2.asInstanceOf[Term[A2]])) else None
  }
}


trait InfixFun[A1, A2, R] extends FunTerm2[A1, A2, R] {
  def symbol: String
  def javaExpr(arg1: String, arg2: String) = "(" + arg1 + " " + symbol + " " + arg2 + ")"
}

trait UnaryFun[A1, R] extends FunTerm1[A1, R] {
  def symbol: String
  def javaExpr(arg1: String) = symbol + arg1
}

object And extends Constant((x: Boolean, y: Boolean) => x && y) with InfixFun[Boolean, Boolean, Boolean] {
  def symbol = "&&"
}

object AndN extends Constant((args: Seq[Boolean]) => args.forall(identity(_))) {
  override def toString = "and"
}

object OrN extends Constant((args: Seq[Boolean]) => args.exists(identity(_)))

object Implies extends Constant((x: Boolean, y: Boolean) => !x || y) with InfixFun[Boolean, Boolean, Boolean] {
  def symbol = "==>"
}

object Eq extends Constant((a1: Any, a2: Any) => a1 == a2) with InfixFun[Any, Any, Boolean] {
  def symbol = "==="
}

object IntAdd extends Constant((x: Int, y: Int) => x + y) with InfixFun[Int, Int, Int] {
  def symbol = "+"
}

object VecAdd extends Constant((x: Vec, y: Vec) => x + y) with InfixFun[Vec, Vec, Vec] {
  def symbol = "+"
  override def javaExpr(arg1: String, arg2: String) = "Vec$.MODULE$.sum(new Vec[]{%s,%s})".format(arg1, arg2)
}

object VecDot extends Constant((x: Vec, y: Vec) => x dot y) with InfixFun[Vec, Vec, Double] {
  def symbol = "dot"
  override def javaExpr(arg1: String, arg2: String) = "%s.dot(%s)".format(arg1, arg2)
}


object DoubleTimes extends Constant((x: Double, y: Double) => x * y) with InfixFun[Double, Double, Double] {
  def symbol = "*"
}

object DoubleAdd extends Constant((x: Double, y: Double) => x + y) with InfixFun[Double, Double, Double] {
  def symbol = "+"
}


object Constants {
  val Zero = Constant(0.0)
  val One = Constant(1.0)

  val VecZero = Constant(Vec.zero)
}


object DoubleAddN extends Constant((args: Seq[Double]) => args.sum) with FunTerm1[Seq[Double], Double]

object DoubleTimesN extends Constant((args: Seq[Double]) => args.product) with FunTerm1[Seq[Double], Double]

object ParameterVectorAddN
  extends Constant((args: Seq[ParameterVector]) => args.fold(new ParameterVector)((l, r) => {l += r })) {
  override def toString = "VSum"
}

object VecAddN
  extends Constant((args: Seq[Vec]) => args.fold(new SparseTroveVec(100))(_ + _)) with FunTerm1[Seq[Vec], Vec] {
  override def toString = "VecAddN"
}


object Iverson extends Constant((x: Boolean) => if (x) 1.0 else 0.0) with UnaryFun[Boolean, Double] {
  override def toString = "I"
  def symbol = "I"
  override def javaExpr(arg1: String) = " %s ? 1.0 : 0.0".format(arg1)
}

case class TupleTerm2[T1, T2](arg1: Term[T1], arg2: Term[T2]) extends Composite[(T1, T2), TupleTerm2[T1, T2]] {

  import Caster._

  def parts = Seq(arg1, arg2)
  def genericCreate(p: Seq[Term[Any]]) = TupleTerm2(p(0), p(1))
  def genericEval(p: Seq[Any]) = (p(0), p(1))
  def default = (arg1.default, arg2.default)
}

case class SeqTerm[+T](args: Seq[Term[T]]) extends Composite[Seq[T], SeqTerm[T]] {
  def parts = args
  def genericCreate(p: Seq[Term[Any]]) = SeqTerm(p.map(_.asInstanceOf[Term[T]]))
  def genericEval(p: Seq[Any]) = p.map(_.asInstanceOf[T])
  def -->(value: Term[Double]) = SingletonVector(this, value)
  def -->(value: Double) = SingletonVector(this, Constant(value))

  override def toString = args.mkString("[", ",", "]")
  def default = args.map(_.default)
}

case class SingletonVecTerm(index: Term[Int], value: Term[Double]) extends Composite[SingletonVec, SingletonVecTerm] {

  import Caster._

  def parts = Seq(index, value)
  def genericCreate(p: Seq[Term[Any]]) = SingletonVecTerm(p(0), p(1))
  def genericEval(p: Seq[Any]) = new SingletonVec(p(0), p(1))
  def default = new SingletonVec(-1, 0.0)
}

case class SingletonVector(args: SeqTerm[Any], value: Term[Double]) extends Composite[ParameterVector, SingletonVector] {
  def parts = Seq(args, value)
  def genericCreate(p: Seq[Term[Any]]) = SingletonVector(p(0).asInstanceOf[SeqTerm[Any]], p(1).asInstanceOf[Term[Double]])
  def genericEval(p: Seq[Any]) = {
    val vector = new ParameterVector
    vector(p(0)) = p(1).asInstanceOf[Double]
    vector
  }
  lazy val default = {
    val vector = new ParameterVector
    val key = args.args.map(_.default)
    vector(key) = 0.0
    vector
  }
}

object Bool extends Dom('bools, Seq(false, true))

object Strings extends Dom('strings, Stream.continually(Random.alphanumeric.take(3).toString))

trait GroundAtom[R] extends Variable[R] {
  def range = pred.range
  def domain = range.values
  def pred: Pred[_,R]
  def args: Seq[Any]
  override def toString = pred.name + args.mkString("(", ",", ")")
}

object GroundAtom {
  def unapply[R](groundAtom: GroundAtom[R]): Option[(Pred[_,R], Seq[Any])] = {
    Some((groundAtom.pred, groundAtom.args))
  }
}

case class GroundAtom1[A1, R](pred: Pred1[A1,R], a1: A1) extends GroundAtom[R] {
  def args = Seq(a1)
}

case class GroundAtom2[A1, A2, R](pred: Pred2[A1,A2,R], a1: A1, a2: A2) extends GroundAtom[R] {
  def args = Seq(a1, a2)
}


trait Pred[F, R] extends Term[F] {
  def genericMapping(args: Seq[Any]): Variable[R]
  def name: Symbol
  def domains: Seq[Dom[Any]]
  def range:Dom[R]
  def variables = CollectionUtil.allTuples(domains.map(_.values)).map(genericMapping(_))
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
  def mapping(a1: A1): GroundAtom1[A1, R] = GroundAtom1(this, a1)
  def apply(a1: A1) = mapping(a1)
  def eval(state: State) = Some((a1: A1) => state(mapping(a1)))
  def default = (a1: A1) => mapping(a1).default
}

case class Pred2[A1, A2, R](name: Symbol, dom1: Dom[A1], dom2: Dom[A2], range: Dom[R] = Bool)
  extends FunTerm2[A1, A2, R] with Pred[(A1, A2) => R, R] {

  def domains = Seq(dom1, dom2)
  def genericMapping(args: Seq[Any]) = mapping(c[A1](args(0)), c[A2](args(1)))
  def mapping(a1: A1, a2: A2) = GroundAtom2(this, a1, a2)
  def apply(a1: A1, a2: A2) = mapping(a1, a2)
  def eval(state: State) = Some((a1: A1, a2: A2) => state(mapping(a1, a2)))
  def default = (a1: A1, a2: A2) => mapping(a1, a2).default
}

case class Dot(arg1: Term[ParameterVector], arg2: Term[ParameterVector]) extends Composite[Double, Dot] {

  //todo: evaluate for each state to get a real vector, then dot product with weights
  //todo: feat = (value1,value2,..)
  def parts = Seq(arg1, arg2)
  def genericCreate(p: Seq[Term[Any]]) = Dot(p(0).asInstanceOf[Term[ParameterVector]], p(1).asInstanceOf[Term[ParameterVector]])
  def genericEval(p: Seq[Any]) = {
    val v1 = p(0).asInstanceOf[ParameterVector]
    val v2 = p(1).asInstanceOf[ParameterVector]
    v1.dot(v2)
  }
  def default = 0.0
}


case class Loglinear(features: Term[Vec], weights: Variable[Vec], base: Term[Double] = Constants.Zero) extends Potential {
  val self = DoubleAdd(VecDot(features, weights), base)
  def hidden = self.variables
  def score(state: State) = self.eval(state).get
  override def substitute(substitution: Substitution) = self.substitute(substitution)
  override def ground = self.ground
}


case class TermPotential(term: Term[Double]) extends Potential {
  def hidden = term.variables
  def score(state: State) = term.eval(state).get
}


object LogicPlayground extends TermImplicits {


  def main(args: Array[String]) {
    val Person = Dom('persons, Range(0, 3))
    val smokes = new Pred1('smokes, Person, Bool) {
      //can be any client-side defined variable that is reused in other templates
      override def mapping(a1: Int) = GroundAtom1(this, a1)
    }
    val cancer = 'cancer := Person -> Bool
    val friends = 'friends := (Person, Person) -> Bool
    val test3 = forall { for (x <- Person) yield smokes(x) }
    println(test3)
    println(forall { for (x <- Person) yield And(cancer(x), smokes(x)) })
    println(forall { for (x <- Person) yield cancer(x) && smokes(x) })
    println(forall { for (x <- Person) yield forall { for (y <- Person) yield cancer(x) |=> smokes(y) } })
    println(forall { for (x <- Person; y <- Person) yield friends(x, y) && friends(y, x) })
    println(forall { for (x <- Person; y <- Person) yield friends(x, y) && friends(y, x) })
    println(forall { for (x <- Person; y <- Person) yield friends(x + 1, 1 + 1 + y) === friends(y, x) })
    println(forall { for (x <- Person; y <- Person) yield (x + y) === (y + x) })
    println(sum { for (x <- Person; y <- Person) yield I((x + y) === (y + x)) })
    //dot { for (s <- Single) yield sum { for (x <- Person) yield smokes(x)}  } //s<-Single can be omitted
    //dot { for (x <- Person) yield sum { for (y <- person) yield friend(x,y) }
    //ppl { 'f1 := dot { for (...) yield ... },
    //      'f2 := det { forall(...) yield ... }, ... }
    //ppl.weights('f1,"word") = 1.0
    //what happens if friends(x) with x \notin dom(friends(x)) ?

    val state = State(Map(smokes(1) -> true, friends(0, 1) -> false))
    println(state(smokes(1)))
    println(state.closed()(smokes(2)))
    val f = forall { for (x <- Person) yield forall { for (y <- Person) yield cancer(x) |=> smokes(y) } }
    val grounded = f.ground
    println(grounded)
    println(reduce(grounded))


  }
}
