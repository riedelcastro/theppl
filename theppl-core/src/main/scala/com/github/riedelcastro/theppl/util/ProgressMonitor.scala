package com.github.riedelcastro.theppl.util

import collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable

/**
 * @author riedelcastro
 */
class ProgressMonitor {

  class Process(val id:Any) {
    var stepsMax = 0
    var stepsCurrent = 0
    var timeStarted = System.currentTimeMillis()
  }

  val processes:mutable.ConcurrentMap[Any,Process]= new ConcurrentHashMap[Any, Process]

  def start(id:Any='default,stepsMax:Int) {
    val process = new Process(id)
    process.stepsCurrent = 0
    process.stepsMax = stepsMax
    processes(id) = process
  }

  def progress(id:Any='default, howmuch:Int = 1) {
    val process = processes(id)
    process.stepsCurrent += howmuch
    if (process.stepsCurrent >= process.stepsMax) {

    }
  }

}

object ProgressMonitor extends ProgressMonitor
