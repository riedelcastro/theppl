package com.github.riedelcastro.theppl

import util.StreamUtil

/**
 * A message is a mapping from variable-value assignments to real numbers.
 *
 * @author sriedel
 */
trait Messages {
  def message[V](variable: Variable[V]): Message[V]
  def msg[V](variable: Variable[V], value: V) = message(variable)(value)
  def apply[V](variable: Variable[V], value: V) = message(variable)(value)
}

trait Message[V] {
  def apply(value: V): Double
  def +(that: Message[V]): Message[V]
  def -(that: Message[V]): Message[V]
  def normalize: Message[V]
}

trait VariableMessage[V] extends Message[V] {
  self =>
  def variable: Variable[V]
  def +(that: Message[V]): Message[V] = {
    new VariableMessage[V] {
      def apply(value: V) = self.apply(value) + that(value)
      def variable = self.variable
    }
  }
  def -(that: Message[V]): Message[V] = {
    new VariableMessage[V] {
      def apply(value: V) = self.apply(value) - that(value)
      def variable = self.variable
    }
  }

  def normalize = {
    val normalizer = math.log(variable.domain.map(v => math.exp(this(v))).sum)
    new VariableMessage[V] {
      def apply(value: V) = self.apply(value) - normalizer
      def variable = self.variable
    }
  }
}

object Message {
  def empty[T] = new Message[T] {
    def +(that: Message[T]) = that
    def -(that: Message[T]) = that
    def apply(value: T) = 0.0
    def normalize = this
  }
}

/**
 * An object with a score.
 */
case class Scored[T](value: T, score: Double)

object Messages {
  val empty = new Messages {
    def message[V](variable: Variable[V]) = Message.empty[V]
  }
  def fromFunction(f: (Variable[Any], Any) => Double) = new Messages {
    def message[V](v: Variable[V]) = new VariableMessage[V] {
      def apply(value: V) = f(v, value)
      def variable = v
    }
  }
  def fromMap(f: scala.collection.Map[(Variable[Any], Any), Double]) = new Messages {
    def message[V](v: Variable[V]) = new VariableMessage[V] {
      def apply(value: V) = f(v, value)
      def variable = v
    }
  }

}

class SingletonMessages[Value](val variable: Variable[Value], val value: Value, val msg: Double) extends Messages {
  self =>
  def message[V](v: Variable[V]) = new VariableMessage[V] {
    def apply(value: V) = if (v == self.variable && value == self.value) msg else 0.0
    def variable = v
  }
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

  /**
   * Iterates over all states for the given variables
   */
  def allStates(variables: Seq[Variable[Any]]) = {
    val domains = variables.map(_.domain).toSeq
    val tuples = StreamUtil.allTuples(domains)
    val states = tuples.map(State(variables, _))
    states
  }

}

/**
 * A State assigns a value to each variable. This assignment also defines a Message in which
 * gives score 1.0 to the variable-value pairs defined in this state, and 0.0 otherwise.
 */
trait State extends Messages {
  self =>
  def apply[V](variable: Variable[V]): V = get(variable).get

  def get[V](variable: Variable[V]): Option[V]

  def closed = new State {
    def get[V](variable: Variable[V]) = self.get(variable).orElse(Some(variable.default))
  }
  def message[V](v: Variable[V]) = new Message[V] {
    thisMsg =>
    def apply(value: V) = if (get(v) == Some(value)) 1.0 else 0.0
    def +(that: Message[V]) = new VariableMessage[V] {
      def apply(value: V) = if (get(v) == Some(value)) 1.0 + that(value) else that(value)
      def variable = v
    }

    def -(that: Message[V]) = new VariableMessage[V] {
      def apply(value: V) = if (get(v) == Some(value)) 1.0 - that(value) else -that(value)
      def variable = v
    }
    def normalize = new VariableMessage[V] {
      def apply(value: V) = thisMsg(value) - 1.0
      def variable = v
    }
  }
}


trait Marginals extends Messages {
  def marginals[V](variable: Variable[V], value: V) = msg(variable, value)
}