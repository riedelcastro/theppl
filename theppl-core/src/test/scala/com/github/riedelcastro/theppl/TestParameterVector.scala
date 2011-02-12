package com.github.riedelcastro.theppl;

import org.junit._
import Assert._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

/*
* @author sriedel
*/
class TestParameterVector {

  @Test
  def testIO {
    val x = new ParameterVector
    x(Feat('test1)) = 1.0
    x(Feat('test2)) = 1.0
    val out = new ByteArrayOutputStream(1000)
    x.save(out)
    val in = new ByteArrayInputStream(out.toByteArray)
    val y = new ParameterVector
    y.load(in)
    assertEquals(x.values, y.values)
  }

  @Test
  def testDotProduct {
    val x1 = new ParameterVector(Seq(Feat('f1),Feat('f2)))
    val x2 = new ParameterVector(Seq(Feat('f2),Feat('f3)))
    assertEquals(1.0, x1 dot x2)
  }
}