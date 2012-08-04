package com.github.riedelcastro.theppl

import collection.mutable.HashMap
import util.{ArrayImports, StreamUtil}
import ArrayImports._

/**
 * A message is a mapping from variable-value assignments to real numbers.
 *
 * @author sriedel
 */
trait Messages {
  def message[V](variable: Variable[V]): Message[V]
  def msg[V](variable: Variable[V], value: V) = message(variable)(value)
  def apply[V](variable: Variable[V], value: V) = message(variable)(value)

  def toArrays: (IndexedSeq[Variable[Any]], IndexedSeq[Array[Double]]) = null

  def toString(variables: TraversableOnce[Variable[Any]]) = {
    variables.map(v => "%s\n%s".format(v, message(v).toString)).mkString("\n")
  }
}

trait MessageVar[V] {
  def variable: Variable[V]
  def :=(msg: Message[V])
  def :=(msgVar: MessageVar[V]) { this := msgVar() }
  def apply(): Message[V]
  def +=(msg: Message[V]) { this := apply() + msg }
  def ++=(msgs: Iterable[Message[V]]) { this := msgs.foldLeft[Message[V]](Message.empty[V](variable))(_ + _) }
}

class ArrayMessage[V](val variable: Variable[V], array: Array[Double]) extends Message[V] {
  lazy val value2index = variable.domain.zipWithIndex.toMap
  def apply(value: V) = array(value2index(value))

  override def +(that: Message[V]) = {
    new ArrayMessage[V](variable, array + that.toArray)
  }
  override def -(that: Message[V]) = {
    new ArrayMessage[V](variable, array - that.toArray)
  }
  override def materialize = this
  override def dot(that: Message[V]) = array.dot(that.toArray)
  override def map(f: (Double) => Double) = new ArrayMessage[V](variable, array.map(f(_)))

  override def norm1 = array.absNormalize()
  override def norm2 = array.twoNorm
  override def toArray = array
}

class ArrayMessageVar[V](val variable: Variable[V]) extends MessageVar[V] {
  val array = Array.ofDim[Double](variable.domain.size)
  def apply() = new ArrayMessage[V](variable, array.copy)
  def :=(msg: Message[V]) { array.set(msg.toArray) }
  override def +=(msg: Message[V]) { array.incr(msg.toArray) }
  override def ++=(msgs: Iterable[Message[V]]) { msgs.foreach(m => array.incr(m.toArray)) }

}

trait Message[V] {
  self =>
  def variable: Variable[V]
  def apply(value: V): Double
  def toArray: Array[Double] = variable.domain.map(apply(_)).toArray
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
    this.map(_ - normalizer)
  }

  def map(f: Double => Double) = {
    new Message[V] {
      def apply(value: V) = f(self(value))
      def variable = self.variable
    }
  }
  def exp = map(math.exp(_))

  def norm1 = variable.domain.view.map(v => math.abs(this(v))).sum
  def norm2 = math.sqrt(variable.domain.view.map(v => math.pow(this(v), 2.0)).sum)


  def entropy = variable.domain.view.map(v => {
    val score = this(v)
    val prob = math.exp(score)
    -prob * score
  }).sum

  def dot(that: Message[V]) = {
    variable.domain.view.map(v => this(v) * that(v)).sum
  }

  def argmax = variable.domain.maxBy(apply(_))

  def offsetDefault(offset: Double) = new Message[V] {
    def variable = self.variable
    def apply(value: V) = if (value == variable.default) self(value) + offset else self(value)
  }

  override def toString = {
    variable.domain.view.map(v => "%20s %8.4f".format(v, this(v))).mkString("\n")
  }

  def materialize: Message[V] = new Message[V] {
    def variable = self.variable
    val map = new HashMap[V, Double]
    def apply(value: V) = map.getOrElseUpdate(value, self(value))
  }

}


object Message {

  def binary(v: Variable[Boolean], trueScore: Double, falseScore: Double = 0.0) = new Message[Boolean] {
    def variable = v
    def apply(value: Boolean) = if (value) trueScore else falseScore
  }

  def empty[T](v: Variable[T]) = new Message[T] {
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

  def fromArrays(variables: IndexedSeq[Variable[Any]], arrays: IndexedSeq[Array[Double]]) = {
    val varMap = variables.zipWithIndex.toMap
    new Messages {
      def message[V](v: Variable[V]) = {
        val array = arrays(varMap(v))
        val valMap = v.domain.zipWithIndex.toMap
        new Message[V] {
          def variable = v
          def apply(value: V) = array(valMap(value))
        }
      }
      override def toArrays = (variables, arrays)
    }
  }

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

  def value[V](variable: Variable[V]) = apply(variable)

  def get[V](variable: Variable[V]): Option[V]

  /**
   * By default states have open world semantics: variables without an assignment return None as result
   * of calling get. This method returns a closed world version of a state: for unassigned variables
   * get returns Some([default state for that variable].
   * @return a closed world version of the given state.
   */
  def closed = new State {
    def get[V](variable: Variable[V]) = self.get(variable).orElse(Some(variable.default))
  }

  /**
   * Overlays this state over the given state. This may not be a good idea to use when adding several states.
   * @param state the state to "underlay".
   * @return A state that returns the value assigned to the variable, if such value exists,
   *         or the value assigned to the variable in the passed state.
   */
  def +(state: State) = new State {
    def get[V](variable: Variable[V]) = self.get(variable).orElse(state.get(variable))
  }

  def message[V](v: Variable[V]) = new Message[V] {
    thisMsg =>
    def apply(value: V) = if (get(v) == Some(value)) 1.0 else 0.0
    def variable = v
  }
}

