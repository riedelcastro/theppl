package com.github.riedelcastro.theppl.util

import org.apache.log4j.Logger

/**
 * A HasLogger object has a logger object it can log to. This logger will
 * be named after the class.
 * @author sriedel
 */
trait HasLogger {

  /**
   * The logger of this object. Uses the class name as logger name.
   */
  val logger = Logger.getLogger(loggerName)

  /**
   * Returns the name to use for the logger.
   */
  def loggerName = getClass.getSimpleName

  /**
   * creates an object that lazily evaluates the given function when `toString` is called.
   */
  def lazyString(func: => String) = new Object {
    override def toString: String = func
  }

}