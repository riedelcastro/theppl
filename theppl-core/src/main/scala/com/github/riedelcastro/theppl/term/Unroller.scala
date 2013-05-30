package com.github.riedelcastro.theppl.term

import scala.collection.mutable
import com.github.riedelcastro.theppl.{State, Variable}

/**
 * @author Sebastian Riedel
 */
object Unroller {

  def unrollVecSum(term: Term[Vec]): Seq[Term[Vec]] = {
    term match {
      case Conditioned(arg, condition) => unrollVecSum(arg).map(Conditioned(_, condition))
      case VecAdd(arg1, arg2) => unrollVecSum(arg1) ++ unrollVecSum(arg2)
      case VecAddN(SeqTerm(args)) => args.flatMap(unrollVecSum)
      case q: QuantifiedVecSum => unrollVecSum(q.groundConditioned) //eager
      case _ => Seq(term)
    }
  }

  def unrollDoubleSum(term: Term[Double]): Seq[Term[Double]] = {
    term match {
      case Conditioned(arg, condition) => unrollDoubleSum(arg).map(Conditioned(_, condition))
      case DoubleAddN(SeqTerm(args)) => args.flatMap(unrollDoubleSum)
      case DoubleAdd(arg1, arg2) => unrollDoubleSum(arg1) ++ unrollDoubleSum(arg2)
      case q: QuantifiedSum => unrollDoubleSum(q.groundConditioned)
      case l: Loglinear => unrollAndGroupLogLinear(l)
      case _ => Seq(term)
    }
  }

  /**
   * Expands/Factorizes a loglinear term into a sequence of additive loglinear terms
   * @param loglinear the input loglinear term
   * @return a sequence of loglinear terms such that its sum equals the input loglinear term
   */
  def unrollAndGroupLogLinear(loglinear: Loglinear): Seq[Loglinear] = {
    //naive way: unroll sums in features and base, then collect terms with same variables
    val feats = unrollVecSum(loglinear.features).map(Simplifier.simplify)
    val bases = unrollDoubleSum(loglinear.base).map(Simplifier.simplify)

    //cluster according to variables
    val vars2Feats = new mutable.HashMap[Iterable[Variable[Any]], mutable.Set[Term[Vec]]] with mutable.MultiMap[Iterable[Variable[Any]], Term[Vec]]
    val vars2Bases = new mutable.HashMap[Iterable[Variable[Any]], mutable.Set[Term[Double]]] with mutable.MultiMap[Iterable[Variable[Any]], Term[Double]]

    for (feat <- feats) vars2Feats.addBinding(feat.variables, feat)
    for (base <- bases) vars2Bases.addBinding(base.variables, base)

    //now turn terms for the same variable set into sums and turn these into loglinear terms
    val loglinears = for (variables <- vars2Feats.keySet ++ vars2Bases.keySet) yield {
      val feat = vars2Feats.get(variables).map(t => if (t.size == 1) t.head else VecAddN(SeqTerm(t.toSeq))).getOrElse(Constants.VecZero)
      val base = vars2Bases.get(variables).map(t => if (t.size == 1) t.head else DoubleAddN(SeqTerm(t.toSeq))).getOrElse(Constants.Zero)
      Loglinear(feat, loglinear.weights, base)
    }
    loglinears.toSeq.filter(l => l.features != Constants.VecZero || l.base != Constants.Zero )
  }

}

object Simplifier {
  def simplify[T](term: Term[T]): Term[T] = {
    term match {
      case Conditioned(t,c) => Conditioned(simplify(t),c)
      case t if (!t.isConstant && t.variables.isEmpty) => Constant(t.eval(State.empty).get)
      case c: Composite[_, _] => {
        val args = c.parts.map(simplify)
        c match {
          case Applied(p: Pred[_, _], _) if (args.drop(1).forall(_.isConstant)) => p.genericMapping(args.drop(1).map(_.eval(State.empty).get)).asInstanceOf[Variable[T]]
          case _ => c.genericCreate(args).asInstanceOf[Term[T]]
        }
      }
      case _ => term
    }
  }
}