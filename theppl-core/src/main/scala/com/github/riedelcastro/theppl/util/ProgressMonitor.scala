package com.github.riedelcastro.theppl.util

import collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable

/**
 * @author riedelcastro
 */
class ProgressMonitor {

  def progressString(fractionDone:Double, ticks:Int = 20) = {
    val ticksDone = (fractionDone * ticks).toInt
    val format = "[%-" + ticks + "s]%7.2f%%"
    format.format(Range(0,ticksDone).view.map(i => "=").mkString, fractionDone * 100)
  }

  class Process(val id:Any, val name:String) {
    var stepsMax = 0
    var stepsCurrent = 0
    var roundsMax = 1
    var roundsCurrent = 0
    var timeStarted = System.currentTimeMillis()

    def printStart() {
      println("Starting %s with %d rounds and %d steps for each".format(name,roundsMax,stepsMax))
//      print("123")
//      print("\u0008")
//      for (i <- 0 until 1000000000; if (i % 10000000 == 0))
//        print(i + "\r")
//      println("")
    }

    def printProgress() {
      val fineInterval = math.max(stepsMax / 100, 1)
      if (stepsCurrent % fineInterval == 0 || stepsCurrent == stepsMax)
        print("\r" + progressString(stepsCurrent / stepsMax.toDouble, 40))
        //print(".")
//      if (stepsCurrent % coarseInterval == 0) print(" ")

    }

  }

  val processes:mutable.ConcurrentMap[Any,Process]= new ConcurrentHashMap[Any, Process]

  def start(id:Any='default,name:String="default",stepsMax:Int,roundsMax:Int = 1) {
    val process = new Process(id,name)
    process.stepsCurrent = 0
    process.stepsMax = stepsMax
    process.roundsMax = roundsMax
    processes(id) = process
    process.printStart()
  }

  def progress(id:Any='default, howmuch:Int = 1) {
    val process = processes(id)
    process.stepsCurrent += howmuch
    if (process.stepsCurrent > process.stepsMax) {
      print("\n")
      process.roundsCurrent += 1
      process.stepsCurrent = 0
    }
    process.printProgress()
    if (process.roundsCurrent > process.roundsMax) {
      processes.remove(process.id)
    }
  }

}

object ProgressMonitor extends ProgressMonitor
