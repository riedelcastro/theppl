package com.github.riedelcastro.theppl.term

import gnu.trove.map.custom_hash.TObjectIntCustomHashMap
import gnu.trove.strategy.HashingStrategy
import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable
import gnu.trove.procedure.TObjectIntProcedure

/**
 * @author Sebastian Riedel
 */
class Index {

  class ArrayHashing extends HashingStrategy[Array[AnyRef]] {
    def computeHashCode(arg: Array[AnyRef]) = util.Arrays.deepHashCode(arg)
    def equals(o1: Array[AnyRef], o2: Array[AnyRef]) = util.Arrays.deepEquals(o1,o2)
  }

  private val map = new TObjectIntCustomHashMap[Array[AnyRef]](new ArrayHashing)

  def index(args:Array[AnyRef]):Int = {
    map.adjustOrPutValue(args,0,map.size)
  }

  def inverse() = {
    val result = new mutable.HashMap[Int,Array[AnyRef]]
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = {result(b) = a; true}
    })
    result
  }

  def apply(args:Term[Any]*) = Indexed(this,SeqTerm(args))

  override def toString = {
    val result = new mutable.StringBuilder()
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = {result.append("%40s -> %d\n".format(a.mkString(" , "),b)); true}
    })
    result.toString()
  }

}

case class Indexed(index:Index, key:SeqTerm[Any]) extends Composite[Int,Indexed] {
  import Caster._

  def parts = key.args
  def genericCreate(p: Seq[Term[Any]]) = Indexed(index,SeqTerm(p))
  def genericEval(p: Seq[Any]) = {
    index.index(p.toArray)
  }
  def default = -1


}