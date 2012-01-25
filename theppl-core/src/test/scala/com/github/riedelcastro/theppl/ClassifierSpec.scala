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
      case class Var(index: Int) extends Variable[Int] {
        def domain = Seq(1,2)
      }
      trait TestClassifier extends Classifier[Int] {
        type LabelType = Int
        type LabelVariableType = Var
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context: Int) = vector(context)
        def variable(context: Int) = Var(context)
      }
      val classifier = new TestClassifier {}
      val copy = new TestClassifier {}
      classifier.weights(Feat(1,1)) = 1.0

      val out = new ByteArrayOutputStream(1000)
      classifier.save(out)

      val in = new ByteArrayInputStream(out.toByteArray)
      copy.load(in)

      copy.weights(Feat(1,1)) must be (1.0 plusOrMinus 0.00001)
      copy.weights.size must be (1)

    }
  }

}