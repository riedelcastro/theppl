package com.github.riedelcastro.theppl

import util.StreamUtil
import collection.mutable.HashMap

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
  self =>
  def variable: Variable[V]
  def apply(value:V):Double
  def +(that: Message[V]): Message[V] = {
    new Message[V] {
      def apply(value: V) = self.apply(value) + that(value)
      def variable = self.variable
    }
  }
  def -(that: Message[V]): Message[V] = {
    new Message[V] {
      def apply(value: V) = self.apply(value) - that(value)
      def variable = self.variable
    }
  }

  def negate = new Message[V] {
    def apply(value: V) = -self.apply(value)
    def variable = self.variable
  }
  def normalize = {
    val normalizer = math.log(variable.domain.map(v => math.exp(this(v))).sum)
    new Message[V] {
      def apply(value: V) = self.apply(value) - normalizer
      def variable = self.variable
    }
  }
  
  def map(f:Double=>Double) = {
    new Message[V] {
      def apply(value: V) = f(self(value))
      def variable = self.variable
    }
  }
  
  def norm1 = variable.domain.view.map(v => math.abs(this(v))).sum
  def norm2 = math.sqrt(variable.domain.view.map(v => math.pow(this(v),2.0)).sum)


  def entropy = variable.domain.view.map(v => {
    val score = this(v)
    val prob =  math.exp(score)
    -prob * score
  }).sum

  def dot(that:Message[V]) = {
    variable.domain.view.map(v => this(v) * that(v)).sum
  }

  override def toString = {
    variable.domain.view.map(v => "%20s %8.4f".format(v,this(v))).mkString("\n")
  }

  def materialize = new Message[V] {
    def variable = self.variable
    val map = new HashMap[V,Double]
    def apply(value: V) = map.getOrElseUpdate(value, self(value))
  }
  
}

  


object Message {

  def binary(v:Variable[Boolean], trueScore:Double, falseScore:Double = 0.0) = new Message[Boolean]{
    def variable = v
    def apply(value: Boolean) = if (value) trueScore else falseScore
  }

  def empty[T](v:Variable[T]) = new Message[T] {
    override def +(that: Message[T]) = that
    override def -(that: Message[T]) = that.negate
    def apply(value: T) = 0.0
    override def normalize = this
    override def negate = this
    def variable = v
  }
}

/**
 * An object with a score.
 */
case class Scored[T](value: T, score: Double)

object Messages {
  val empty = new Messages {
    def message[V](variable: Variable[V]) = Message.empty(variable)
  }
  def fromFunction(f: (Variable[Any], Any) => Double) = new Messages {
    def message[V](v: Variable[V]) = new Message[V] {
      def apply(value: V) = f(v, value)
      def variable = v
    }
  }
  def fromMap(f: scala.collection.Map[(Variable[Any], Any), Double]) = new Messages {
    def message[V](v: Variable[V]) = new Message[V] {
      def apply(value: V) = f(v, value)
      def variable = v
    }
  }

}

class SingletonMessages[Value](val variable: Variable[Value], val value: Value, val msg: Double) extends Messages {
  self =>
  def message[V](v: Variable[V]) = new Message[V] {
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
    def variable = v
  }
}

