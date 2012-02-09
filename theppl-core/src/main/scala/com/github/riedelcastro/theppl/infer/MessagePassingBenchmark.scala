package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl._
import util.Timer


/**
 * @author sriedel
 */
object MessagePassingBenchmark {

  def main(args: Array[String]) {
    val dom = Range(0,4)
    case class Var(id:Int) extends Variable[Int] {
      def domain = dom
    }
    case class Edge(x:Var, y:Var, map:Map[(Int, Int),Double]) extends FeatureModel {
      val hidden = IndexedSeq(x,y)
      def score(state: State) = map(state(x)->state(y))
      def features(state: State) = new ParameterVector()
    }
    
    def pot() = (for (i <- dom; j <- dom) yield (i, j) -> math.random).toMap
    
    val variables = for (i <- 0 until 5) yield Var(i)
    val edges = for (v1 <- variables; v2 <- variables; if (v1.id < v2.id)) yield Edge(v1,v2,pot())

    val sum = new FeatureSumModel {
      def featureArgs = Seq.empty
      def otherArgs = edges
    }
    val timer = new Timer
    for (iteration <- 0 until 100) {
      val expectator = timer.time("create",sum.defaultExpectator())
      val expectations = timer.time("infer",expectator.expectations())
      timer.time("marginals", expectations.logMarginals)
      timer.time("features", expectations.featureExpectations)

    }

    println(timer)
    
  }

}
