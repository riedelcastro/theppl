package com.github.riedelcastro.theppl

import collection.Map
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author sriedel
 */
class ParameterVector {

  def this(keys: Iterable[Feat]) = {
    this ()
    for (key <- keys) _values(key) = 1.0
  }

  private val _values = new HashMap[Feat, Double] {
    override def default(key: Feat) = 0.0
  }

  def values: Map[Feat, Double] = _values

  def update(key: Feat, value: Double) = {
    _values(key) = value
  }

  def apply(key: Feat): Double = _values(key)

  def add(that: ParameterVector, scale: Double) = {
    for ((key, value) <- that.values) {
      _values(key) = _values(key) + scale * value
    }
  }
  def dot(that: ParameterVector): Double = {
    var result = 0.0
    for ((key, value) <- values) {
      result += value * that(key)
    }
    result
  }

  def conjoin(that: ParameterVector): ParameterVector = {
    val result = new ParameterVector
    for ((key, value) <- this.values; (thatKey, thatValue) <- that.values) {
      result(key & thatKey) = value * thatValue
    }
    result
  }

  def stringRep(prefix: String) = {
    _values.map({
      case (key, value) => "%s%-30s: %6.2f".format(prefix, key, value)
    }).mkString("\n")
  }


}

class AveragingParameterVector extends ParameterVector {

  var updateCount = 0
  var consistifiedAt = 0
  val average = new ParameterVector

  val oldCounts = new HashMap[Feat, Int] {
    override def default(key: Feat): Int = 0
  }

  override def add(x: ParameterVector, scale: Double) {
    updateCount += 1
    for ((key, value) <- x.values) {
      val oldValue = apply(key)
      val newValue = oldValue + scale * value
      val oldCount = oldCounts(key)
      val oldAvgValue = average(key)
      val newAverage = (oldAvgValue * oldCount + (updateCount - oldCount - 1) * oldValue + newValue) / updateCount
      average(key) = newAverage
      this(key) = newValue
      oldCounts(key) = updateCount
    }
  }
  def consistify() {
    if (consistifiedAt < updateCount) {
      for (key <- values.keys) {
        val oldCount = oldCounts(key)
        val oldValue = apply(key)
        val oldAverage = average(key)
        val newAverage = ((oldAverage * oldCount) + (updateCount - oldCount) * oldValue) / updateCount
        average(key) = newAverage
        oldCounts(key) = updateCount
      }
      consistifiedAt = updateCount
    }
  }

}


object ParameterVector {
  def fromFeats(feats: Iterable[Feat]) = {
    new ParameterVector(feats)
  }
  def fromPairs(feats: (Any, Any)*) = {
    new ParameterVector(feats.map({
      case (key, value) => Feat(key, value)
    }))
  }
}


class Feat extends ArrayBuffer[Symbol] {
  def &(that: Feat) = {
    val feat = new Feat
    feat ++= this
    feat ++= that
    feat
  }
  override def toString() = map(_.name).mkString(" ")
}

object Feat {
  def apply(feats: Any*): Feat = {
    val result = new Feat
    for (feat <- feats) result += Symbol(feat.toString)
    result
  }
}


class GlobalParameterVector {

  type Path = Seq[Module]

  def this(module: Module, params: ParameterVector) {
    this ()
    _params(Seq(module)) = params
  }

  private val _params = new HashMap[Path, ParameterVector]

  def params: Map[Path, ParameterVector] = _params

  def add(that: GlobalParameterVector, scale: Double) {
    for ((path, thatVector) <- that.params; vector = _params.getOrElseUpdate(path, new ParameterVector)) {
      vector.add(thatVector, scale)
    }
  }

  def dot(that: GlobalParameterVector): Double = {
    var result = 0.0
    for ((path, vector) <- _params; thatVector <- that.params.get(path)) {
      result += vector dot thatVector
    }
    result
  }
  override def toString = {
    val perPath = for ((path, vector) <- _params) yield {
      path.toString + "\n" + vector.stringRep("\t")
    }
    perPath.mkString("\n")
  }
}
