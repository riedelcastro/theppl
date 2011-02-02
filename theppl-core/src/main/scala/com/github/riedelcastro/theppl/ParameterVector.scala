package com.github.riedelcastro.theppl

import collection.mutable.HashMap
import collection.Map

/**
 * @author sriedel
 */
class ParameterVector {

  def this(keys:Iterable[Any]) = {
    this()
    for (key <- keys) _values(key) = 1.0
  }

  private val _values = new HashMap[Any,Double] {
    override def default(key: Any) = 0.0
  }

  def values:Map[Any,Double] = _values

  def update(key:Any, value:Double) = {
    _values(key) = value
  }

  def apply(key:Any):Double = _values(key)

  def add(that:ParameterVector, scale:Double) = {
    for ((key,value) <- that.values) {
      _values(key) = _values(key) + scale * value
    }
  }
  def dot(that:ParameterVector):Double = {
    var result = 0.0
    for ((key,value) <- values) {
      result += value * that(key)
    }
    result
  }

  def conjoin(that:ParameterVector):ParameterVector = {
    val result = new ParameterVector
    for ((key,value) <- this.values; (thatKey,thatValue) <- that.values){
      result(key -> thatKey) = value * thatValue
    }
    result
  }

}

class GlobalParameterVector {

  type Path = Seq[Module]

  def this(module:Module, params:ParameterVector) {
    this()
    _params(Seq(module)) = params
  }

  private val _params = new HashMap[Path,ParameterVector]

  def params:Map[Path, ParameterVector] = _params

  def add(that:GlobalParameterVector, scale:Double) {
    for ((path,vector) <- _params; thatVector <- that.params.get(path)) {
      vector.add(thatVector, scale)
    }
  }

  def dot(that:GlobalParameterVector):Double = {
    var result = 0.0
    for ((path,vector) <- _params; thatVector <- that.params.get(path)) {
      result += vector dot thatVector
    }
    result
  }
}
