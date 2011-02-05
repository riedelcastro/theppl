package com.github.riedelcastro.theppl.nlp

import com.github.riedelcastro.theppl.Atom

/**
 * @author sriedel
 */
class Token {
}

class Sentence {

}

class Document {

}

case class Tag(token:Token) extends Atom('tag,token)
case class Chunk(token:Token) extends Atom('chunk,token)
case class NER(token:Token) extends Atom('ner,token)