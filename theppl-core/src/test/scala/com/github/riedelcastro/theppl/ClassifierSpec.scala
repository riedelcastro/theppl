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
      trait TestClassifier extends Classifier {
        type Context = Int
        type LabelType = Int
        type LabelVariableType = Var
        def labelFeatures(label: LabelType) = vector(label)
        def contextFeatures(context: Context) = vector(context)
        def variable(context: Context) = Var(context)
      }
      val classifier = new TestClassifier {}
      val copy = new TestClassifier {}
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