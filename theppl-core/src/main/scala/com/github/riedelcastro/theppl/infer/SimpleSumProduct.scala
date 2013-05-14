package com.github.riedelcastro.theppl.infer

import com.github.riedelcastro.theppl.term.{Unroller, Loglinear}

/**
 * @author Sebastian Riedel
 */
object SimpleSumProduct {

  def infer(model:Loglinear) = {
    val unrolled = Unroller.unrollAndGroupLogLinear(model)
    //build graph
    //create messages holders
    ???
  }

}
