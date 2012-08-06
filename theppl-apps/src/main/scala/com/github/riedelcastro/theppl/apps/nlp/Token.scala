package com.github.riedelcastro.theppl.apps.nlp

import org.riedelcastro.frontlets.Frontlet


/**
 * @author sriedel
 */
class Token extends Frontlet {
  val charBegin = IntSlot("charBegin")
  val charEnd = IntSlot("charEnd")
  val word = StringSlot("word")
  val lemma = StringSlot("lemma")
  val document = FrontletSlot("document",() => new Document)
  def source() = document().source().substring(charBegin(),charEnd())
}

class Sentence extends Frontlet {
  val tokenBegin = IntSlot("tokenBegin")
  val tokenEnd = IntSlot("tokenEnd")
  val document = FrontletSlot("document", () => new Document)
  def tokens() = document().tokens().slice(tokenBegin(),tokenEnd())
}

class Document extends Frontlet {
  val name = StringSlot("name")
  val source = StringSlot("source")
  val sentences = FrontletListSlot("sentences", () => new Sentence().document(this))
  val tokens = FrontletListSlot("tokens", () => new Token().document(this))

}
