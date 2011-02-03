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

class Tag(token:Token, domain:Iterable[String]) extends Atom('tag,token,domain)
class Chunk(token:Token, domain:Iterable[String]) extends Atom('chunk,token,domain)
class NER(token:Token, domain:Iterable[String]) extends Atom('ner,token,domain)