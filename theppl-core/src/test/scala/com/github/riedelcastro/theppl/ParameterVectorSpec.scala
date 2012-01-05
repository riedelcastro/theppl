package com.github.riedelcastro.theppl;

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers


/*
* @author sriedel
*/
class ParameterVectorSpec extends Spec with MustMatchers {

  describe("A ParameterVector") {
    it("should store and load parameters") {
      val x = new ParameterVector
      x(Feat('test1)) = 1.0
      x(Feat('test2)) = 1.0
      val out = new ByteArrayOutputStream(1000)
      x.save(out)
      val in = new ByteArrayInputStream(out.toByteArray)
      val y = new ParameterVector
      y.load(in)
      x.values must be (y.values)
    }
    
    it("should calculate the dot product with another vector") {
      val x1 = new ParameterVector(Seq(Feat('f1),Feat('f2)))
      val x2 = new ParameterVector(Seq(Feat('f2),Feat('f3)))
      (x1 dot x2) must be (1.0 plusOrMinus 0.00001)
    }

  }

}