package com.github.riedelcastro.theppl.util

import collection.mutable.HashMap

/**
 * Times operations and remembers the measured times.
 * @author sriedel
 */
class Timer {
  val totalTimes = new HashMap[String, Long] {
    override def default(key: String): Long = 0
  }
  val calls = new HashMap[String, Int] {
    override def default(key: String): Int = 0
  }

  def averageTime(label: String): Double = {
    totalTimes(label).toDouble / calls(label)
  }

  def reset() {
    totalTimes.clear()
    calls.clear()
  }

  /**
   * Time the call to the given function, and return the result of the function.
   */
  def time[T](label: String, function: => T): T = {
    val start = System.currentTimeMillis
    val result = function
    val delta = System.currentTimeMillis - start
    calls(label) = calls(label) + 1
    totalTimes(label) = totalTimes(label) + delta
    result
  }

  def timeCall[T](label: String)(function: => T): T = time(label, function)

  override def toString: String = {
    calls.keys.map(key =>
      "====================================\n%-20s:\n".format(key) +
        "%-20s: %-6d\n".format("Calls", calls(key)) +
        "%-20s: %-6f\n".format("Avg Time", averageTime(key))).mkString("\n")
  }
}

