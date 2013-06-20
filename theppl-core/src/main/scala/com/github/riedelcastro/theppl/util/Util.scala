package com.github.riedelcastro.theppl.util

import java.io.{FileInputStream, InputStream, File}
import io.Source
import collection.mutable.ArrayBuffer
import annotation.tailrec
import collection.JavaConversions._
import java.util.Scanner
import com.github.riedelcastro.theppl.{Variable, State}

/**
 * @author sriedel
 */
object Util extends HasLogger {

  def infinity = sys.error("Infinite data structure")

  val infiniteSeq:Seq[Nothing] = new Seq[Nothing] {
    def length = sys.error("Infinite data structure")
    def apply(idx: Int) = sys.error("Infinite data structure")
    def iterator = sys.error("Infinite data structure")
  }

  /**
   * Returns an iterator over strings in the stream, as delimited by the given string
   * @param in the input stream
   * @param delim the delimiter string
   * @return the input stream as iterator of strings, separated by the given delimiter.
   */
  def streamIterator(in: InputStream, delim: String = "\n"): Iterator[String] = {
    new Scanner(in).useDelimiter(delim)
  }

  /** Recursively descend directory, returning a list of files. */
  def files(directory: File): Seq[File] = {
    if (!directory.exists) throw new Error("File " + directory + " does not exist")
    if (directory.isFile) return List(directory)
    val result = new ArrayBuffer[File]
    for (entry: File <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }

  /**
   * turns value that can be null into Options that are None if the value t is null, or Some(t)
   * otherwise.
   */
  def option[T](t: T) = if (t == null) None else Some(t)

  /**
   * Turns integers into options: None if i==-1, or Some(i) otherwise.
   */
  def optMinusOne(i: Int): Option[Int] = if (i == -1) None else Some(i)

  /**
   * Takes a relative path name and finds the corresponding file in the classpath. Returns
   * its absolute path.
   */
  def resolveRelativePathUsingClassPath(path: String): String = {
    val resourceURL = getClass.getResource("/" + path)
    if (resourceURL == null)
      new File(path).getAbsolutePath
    else
      new File(resourceURL.toURI).getAbsolutePath
  }

  /**
   * Execute f on each index in 0..n
   */
  @inline
  def forIndex(n: Int)(f: Int => Unit) {
    var i = 0
    while (i < n) {f(i); i += 1 }
  }

  /**
   * Execute f on n..0
   */
  def forReverseIndex(n: Int)(f: Int => Unit) {
    var i = n - 1
    while (i >= 0) {f(i); i -= 1 }
  }


  /**
   * Loads a resource as stream. This returns either a resource in the classpath,
   * or in case no such named resource exists, from the file system.
   */
  def getStreamFromClassPathOrFile(name: String): InputStream = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(name)
    if (is == null) {
      logger.info("Loaded resource %s from file system".format(name))
      new FileInputStream(name)
    }
    else {
      logger.info("Loaded resource %s from class path".format(name))
      is
    }

  }

  /**
   * Loads a resource as stream. This returns either a resource in the file system,
   * or in case no such named file exists, from the class path system.
   */
  def getStreamFromFileOrClassPath(name: String): InputStream = {
    val file = new File(name)
    if (file.exists) {
      logger.info("Loaded resource %s from file system".format(name))
      new FileInputStream(file)
    } else {
      val is: InputStream = getClass.getClassLoader.getResourceAsStream(name)
      if (is == null) {
        sys.error("Couldn't find resource %s on file system or class path".format(name))
      } else {
        logger.info("Loaded resource %s from class path".format(name))
        is
      }
    }
  }

  /**
   * Creates an iterator over input streams by splitting the source
   * input stream at the given delimiter.
   * todo: this seems undone
   */
  abstract class InputStreamIterator(source: InputStream, delimiter: String) extends Iterator[InputStream] {
    private val bytes = delimiter.getBytes
    private val buffer = Array.ofDim[Int](bytes.length + 1)
    private var current = 0
    private var matched = 0
    private var reachedEnd = false
    private var reachedDelimiter = true
    private var stream = new DelimitedInputStream

    private def readIntoBuffer: Boolean = {
      if (current >= buffer.size) {
        current = 0
      }
      val read = source.read
      buffer(current) = read
      if (read == -1) reachedEnd = true
      true
    }

    def hasNext = !reachedEnd


    def next() = {
      if (!reachedDelimiter) sys.error("Cannot call next before current stream has reached delimiter")
      stream
    }


    class DelimitedInputStream extends InputStream {
      def read = {
        val result = buffer.head
        readIntoBuffer
        if (reachedDelimiter) -1 else result
      }
    }

  }


  /**
   * Bins a number and returns the bin. If number is negative,
   * bin number will be negative.
   */
  def bin(number: Int, bins: Seq[Int]): Int = {
    val abs = scala.math.abs(number)
    val sign = if (number >= 0) 1 else -1
    for (i <- bins) {
      if (abs < i) return sign * i
    }
    sign * (bins.last + 5)
  }

  def loadStrings(file: String): Seq[String] = {
    val res = new ArrayBuffer[String]
    val source = Source.fromFile(file)
    for (line <- source.getLines()) {
      res += line
    }
    res
  }

}

object MathUtil {

  /**
   * Finds the argmax (plus max) of a scored set of elements.
   */
  def argmax[T](elements: Iterable[T], score: T => Double): (T, Double) = {
    var max = Double.NegativeInfinity
    var result: T = null.asInstanceOf[T]
    for (e <- elements) {
      val s = score(e)
      if (s > max) {
        max = s
        result = e
      }
    }
    result -> max
  }
}

object CollectionUtil {
  @tailrec
  def allSubSequences[T](items: Seq[T], tail: Stream[Seq[T]] = Stream(Seq.empty)): Stream[Seq[T]] = {
    if (items.size == 0) tail
    else {
      val head = items.head
      val sequences = tail ++ tail.map(_ :+ head)
      allSubSequences(items.drop(1), sequences)
    }
  }

  @tailrec
  def allTuples[T](domains: Seq[Iterable[T]], tail: Stream[Seq[T]] = Stream(Seq.empty)): Stream[Seq[T]] = {
    if (domains.size == 0) tail
    else {
      val domain = domains.head
      val tuples = tail flatMap (prefix => domain map (prefix :+ _))
      allTuples(domains.drop(1), tuples)
    }
  }

  @tailrec
  def allTuplesIterator[T](domains: Seq[Iterable[T]], tail: Iterator[Seq[T]] = Iterator(Seq.empty)): Iterator[Seq[T]] = {
    if (domains.size == 0) tail
    else {
      val domain = domains.head
      val tuples = tail flatMap (prefix => domain map (prefix :+ _))
      allTuplesIterator(domains.drop(1), tuples)
    }
  }

  def allStates(variables: Seq[Variable[Any]]) = {
    val domains = variables.map(_.domain).toSeq
    val tuples = CollectionUtil.allTuples(domains)
    val states = tuples.map(State(variables, _))
    states
  }

  def allStatesIterator(variables: Seq[Variable[Any]]) = {
    val domains = variables.map(_.domain).toSeq
    val tuples = CollectionUtil.allTuplesIterator(domains)
    val states = tuples.map(State(variables, _))
    states
  }

  def allStatesIterator(variables: Seq[Variable[Any]], fixedVariables: Seq[Variable[Any]], fixedValues: Seq[Any]) = {
    val domains = variables.map(_.domain).toSeq ++ fixedValues.map(Seq(_))
    val tuples = CollectionUtil.allTuplesIterator(domains)
    val states = tuples.map(State(variables ++ fixedVariables, _))
    states
  }


}

object SetUtil {

  case class SetMinus[T](set: Set[T], without: Set[T]) extends Set[T] {
    def contains(elem: T) = set.contains(elem) && !without.contains(elem)
    def +(elem: T) = SetMinus(set + elem, without)
    def -(elem: T) = SetMinus(set, without + elem)
    def iterator = set.iterator.filterNot(without)
  }

  case class Union[T](sets: Set[Set[T]]) extends Set[T] {
    def contains(elem: T) = sets.exists(_.contains(elem))
    def +(elem: T) = Union(sets + Set(elem))
    def -(elem: T) = SetMinus(this, Set(elem))
    def iterator = sets.flatMap(identity).iterator
  }

}
