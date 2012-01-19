package com.github.riedelcastro.theppl

/**
 * A message is a mapping from variable-value assignments to real numbers.
 *
 * @author sriedel
 */
trait Message {
  def msg[V](variable: Variable[V], value: V): Double
}

/**
 * An object with a score.
 */
case class Scored[T](value: T, score: Double)

object Message {
  val emtpy = new Message {
    def msg[V](variable: Variable[V], value: V) = 0.0
  }
  def apply(f:(Variable[Any],Any) =>Double) = new Message {
    def msg[V](variable: Variable[V], value: V) = f(variable,value)
  }
}

class SingletonMessage[Value](val variable: Variable[Value], val value: Value, val msg: Double) extends Message {
  def msg[V](variable: Variable[V], value: V) =
    if (variable == this.variable && value == this.value) msg else 0.0
}

class SingletonState[Value](val variable: Variable[Value], val state: Value) extends State {
  def get[V](variable: Variable[V]) =
    if (variable == this.variable) Some(state.asInstanceOf[V]) else None

  override def toString = variable + " = " + state
}

object State {
  val empty = new State {
    def get[V](variable: Variable[V]) = None
  }

  def singleton[Value](variable: Variable[Value], state: Value) = new SingletonState(variable, state)

  def apply(map: Map[Variable[Any], Any]): State = new State {
    def get[V](variable: Variable[V]) = map.get(variable).asInstanceOf[Option[V]]
  }

  def fromFunction(pf: PartialFunction[Variable[Any], Any]): State = new State {
    def get[V](variable: Variable[V]) = pf.lift(variable).asInstanceOf[Option[V]]
  }


  def apply(variables: Seq[Variable[Any]], tuple: Seq[Any]): State = {
    val map = variables.indices.map(i => variables(i) -> tuple(i)).toMap
    apply(map)
  }

}

/**
 * A State assigns a value to each variable. This assignment also defines a Message in which
 * gives score 1.0 to the variable-value pairs defined in this state, and 0.0 otherwise.
 */
trait State extends Message {
  def apply[V](variable: Variable[V]): V = get(variable).get

  def get[V](variable: Variable[V]): Option[V]

  def msg[V](variable: Variable[V], value: V) = if (get(variable) == Some(value)) 1.0 else 0.0
}


trait Marginals extends Message {
  def marginals[V](variable: Variable[V], value: V) = msg(variable, value)
}