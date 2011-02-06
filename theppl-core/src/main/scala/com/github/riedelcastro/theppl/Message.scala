package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait Message {
  def msg[V](variable: Variable[V], value: V): Double
}

object Message {
  val emtpy = new Message {
    def msg[V](variable: Variable[V], value: V) = 0.0
  }
}

class SingletonMessage[Value](val variable:Variable[Value], val value:Value, val msg:Double) extends Message {
  def msg[V](variable: Variable[V], value: V) =
    if (variable == this.variable && value == this.value) msg else 0.0
}

class SingletonState[Value](val variable:Variable[Value], val state:Value) extends State {
  def get[V](variable: Variable[V]) =
    if (variable == this.variable) Some(state.asInstanceOf[V]) else None
  override def toString = variable + " = " + state
}

object State {
  val empty = new State {
    def get[V](variable: Variable[V]) = None
  }
  def singleton[Value](variable:Variable[Value],state:Value) = new SingletonState(variable,state)
}

trait State extends Message {
  def apply[V](variable:Variable[V]):V = get(variable).get
  def get[V](variable: Variable[V]): Option[V]
  def msg[V](variable: Variable[V], value: V) = if (get(variable) == Some(value)) 1.0 else 0.0
}

trait Marginals extends Message {
  def marginals[V](variable: Variable[V], value: V) = msg(variable, value)
}