package com.github.riedelcastro.theppl.apps

import com.github.riedelcastro.theppl.Module

/**
 * A coreference module that uses delayed column generation for inference
 * @author sriedel
 */
trait DCCoref extends Module {

  type Mention
  
  def mentions(context:Context):Seq[Mention]

  trait DCCorefModel extends Model {

  }

}