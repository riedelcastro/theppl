package com.github.riedelcastro.theppl

/**
 * An Indexer maps keys to integer indices and vice versa.
 * @author sriedel
 */
trait Index[KeyType] {

  def indexOf(key: KeyType): Int
  def keyOf(index: Int): KeyType
  def size:Int

}

/**
 * Provides indices for variables for efficient processing.
 */
trait IndexScheme {
  def index[T](v: Variable[T]): Index[T]

}