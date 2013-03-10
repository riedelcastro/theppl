package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl._
import org.riedelcastro.nurupo.BuilderN
import scala.Tuple3
import scala.Tuple2

/**
 * @author Sebastian Riedel
 */
trait LogicImplicits {

  def $ = Iverson
  def I = Iverson
  def S[V](args:Term[V]*) = SeqTerm(args.toSeq)


  def sum(arg1:Term[ParameterVector],arg2:Term[ParameterVector]) = {
    arg1 match {
      //case FunApp1(ParameterVectorAddN,SeqTerm(args)) => FunApp1(ParameterVectorAddN,SeqTerm(args :+ arg2))
      case _ => FunApp1(ParameterVectorAddN,SeqTerm(Seq(arg1,arg2)))
    }
  }

  implicit def toLogLinear(term:Dot) = term match {
    case Dot(feats,weights:VectorVar[_]) => Loglinear(feats,weights)
    case _ => sys.error(term + " can't be converted into loglinear representation")
  }

  implicit def toPotential(term: Term[Double]) = new TermPotential(term)

  def forall(builder: BuilderN[Variable[Any], Term[Boolean]]) = {
    Forall(builder.arguments, builder.built)
  }

  def sum(builder: BuilderN[Variable[Any], Term[Double]]) = QuantifiedSum(builder.arguments, builder.built)

  def vector(builder: BuilderN[Variable[Any], Term[ParameterVector]]) = QuantifiedVectorSum(builder.arguments, builder.built)


  def dot(builder: BuilderN[Variable[Any], Term[Double]]) = {
    null
  }

  def reduce[T](term: Term[T]): Term[T] = {
    term match {
      case FunApp1(pred@Pred1(_, _, _), Constant(a1)) => pred.mapping(a1)
      case FunApp2(pred@Pred2(_, _, _, _), Constant(a1), Constant(a2)) => pred.mapping(a1, a2)
      case c: Composite[_, _] =>
        val parts = c.parts.map(reduce(_))
        val constants = parts.collect({ case Constant(x) => x })
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

  trait DoubleTermBuilder {
    def arg1: Term[Double]
    def *(arg2: Term[Double]) = DoubleTimes(arg1, arg2)
    def *(arg2: Double) = DoubleTimes(arg1, Constant(arg2))
  }

  trait ParameterVectorTermBuilder {
    def arg1: Term[ParameterVector]
    def dot(arg2: Term[ParameterVector]) = Dot(arg1,arg2)
    def +(arg2:Term[ParameterVector]) = FunApp1(ParameterVectorAddN,SeqTerm(Seq(arg1,arg2)))
  }

  trait SingletonVectorBuilder {
    def arg1: Term[Any]
    def -->(arg2:Term[Double]) = SingletonVector(SeqTerm(Seq(arg1)),arg2)
  }

  trait ValueToSingletonVectorBuilder {
    def arg1: Any
    def -->(arg2:Term[Double]) = SingletonVector(SeqTerm(Seq(Constant(arg1))),arg2)
  }

  implicit def boolTermToBuilder(term: Term[Boolean]): BooleanTermBuilder = new BooleanTermBuilder {
    def arg1 = term
  }

  implicit def intTermToBuilder(term: Term[Int]): IntTermBuilder = new IntTermBuilder {
    def arg1 = term
  }

  implicit def doubleTermToBuilder(term: Term[Double]): DoubleTermBuilder = new DoubleTermBuilder {
    def arg1 = term
  }

  implicit def vectorTermToBuilder(term: Term[ParameterVector]): ParameterVectorTermBuilder = new ParameterVectorTermBuilder {
    def arg1 = term
  }

  implicit def termToSingletonBuilder(term: Term[Any]) = new SingletonVectorBuilder {
    def arg1 = term
  }

  implicit def valueToSingletonBuilder(term: Any) = new ValueToSingletonVectorBuilder {
    def arg1 = term
  }



  //implicit def pairToSingletonVector(pair:Pair[SeqTerm[Any],Term[Double]]) = SingletonVector(pair._1,pair._2)

  //implicit def pairToSingletonVector(pair:Pair[SeqTerm[Any],Term[Double]]) = SingletonVector(pair._1,pair._2)

//  implicit def objectToSeqTerm(obj:Any) = SeqTerm(Seq(Constant(obj)))

  implicit def intToConstant(x: Int) = Constant(x)
  implicit def symbolToConstant(s: Symbol) = Constant(s)
  implicit def parameterVectorToConstant(v: ParameterVector) = Constant(v)



  implicit def intToBuilder(x: Int) = new AnyRef {
    def +(that: Term[Int]) = IntAdd(Constant(x), that)
  }

}

object LogicImplicits extends LogicImplicits