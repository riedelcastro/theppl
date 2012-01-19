package com.github.riedelcastro.theppl.util

import java.io.{FileInputStream, InputStream, File}
import io.Source
import collection.mutable.ArrayBuffer
import annotation.tailrec

/**
 * @author sriedel
 */
object Util extends HasLogger {
  /**Recursively descend directory, returning a list of files. */
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
  def forIndex(n:Int)(f:Int=>Unit) {
    var i = 0
    while (i < n) { f(i); i += 1 }
  }

  /**
   * Execute f on n..0
   */
  def forReverseIndex(n:Int)(f:Int=>Unit) {
    var i = n - 1
    while (i >= 0) { f(i); i -= 1 }
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
   */
  abstract class InputStreamIterator(source:InputStream, delimiter:String) extends Iterator[InputStream] {
    private val bytes = delimiter.getBytes
    private val buffer = Array.ofDim[Int](bytes.length + 1)
    private var current = 0
    private var matched = 0
    private var reachedEnd = false
    private var reachedDelimiter = true
    private var stream = new DelimitedInputStream

    private def readIntoBuffer:Boolean = {
      if (current >= buffer.size){
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
    for (line <- source.getLines) {
      res += line
    }
    res
  }

}

object MathUtil {

  /**
   * Finds the argmax (plus max) of a scored set of elements.
   */
  def argmax[T](elements:Iterable[T], score:T=>Double):(T,Double) = {
    var max = Double.NegativeInfinity
    var result:T = null.asInstanceOf[T]
    for (e <- elements){
      val s = score(e)
      if (s > max){
        max = s
        result = e
      }
    }
    result -> max
  }
}

object StreamUtil {
  @tailrec
  def allSubSequences[T](items:Seq[T], tail:Stream[Seq[T]] = Stream(Seq.empty)):Stream[Seq[T]] = {
    if (items.size == 0) tail else {
      val head = items.head
      val sequences = tail ++ tail.map(_ :+ head)
      allSubSequences(items.drop(1),sequences)
    }
  } 
  
  @tailrec
  def allTuples[T](domains:Seq[Iterable[T]], tail:Stream[Seq[T]] = Stream(Seq.empty)): Stream[Seq[T]] = {
    if (domains.size == 0) tail else {
      val domain = domains.head
      val tuples = tail flatMap (prefix => domain map (prefix :+ _))
      allTuples(domains.drop(1),tuples)
    }
  }
  
}
