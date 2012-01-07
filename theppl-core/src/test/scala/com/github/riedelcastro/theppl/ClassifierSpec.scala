package com.github.riedelcastro.theppl

import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import Imports._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

/**
 * @author sriedel
 */
class ClassifierSpec extends Spec with MustMatchers {

  describe("A Classifier") {
    it("should store and load its weights") {
      case class Var(index: Int) extends Variable[Int]
      val classifier = Classifier[Int, Int, Var](Var(_), Seq(1, 2), vector(_), vector(_))
      val copy = Classifier[Int, Int, Var](Var(_), Seq(1, 2), vector(_), vector(_))
      classifier.weights(Seq.empty) = new ParameterVector(Seq(Feat(1,1)))

      val out = new ByteArrayOutputStream(1000)
      classifier.save(out)

      val in = new ByteArrayInputStream(out.toByteArray)
      copy.load(in)

      copy.weights.params(Seq.empty)(Feat(1,1)) must be (1.0 plusOrMinus 0.00001)
      copy.weights.params(Seq.empty).size must be (1)

    }
  }

}