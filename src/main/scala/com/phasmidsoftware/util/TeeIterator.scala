/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

/**
 * This iterator gets its input from a Iterator[X], which is essentially a parameterless function.
 * The first n elements of the iterator can be passed to the caller of tee.
 *
 * NOTE: this is an odd name for the class. Its purpose appears to be to drop the first n elements of an iterator and make them available via tee.
 *
 * CONSIDER passing the parameter n to the tee method instead of the constructor.
 *
 * @param n  the number of elements to detach.
 * @param xs the input iterator.
 * @tparam X the underlying type.
 */
class TeeIterator[X](n: Int)(xs: Iterator[X]) extends Iterator[X] {
  val tee: Seq[X] = for (_ <- 0 until n if hasNext) yield next()

  def hasNext: Boolean = xs.hasNext

  def next(): X = xs.next()
}

/**
 * TESTME
 *
 * @param xs
 * @tparam X
 */
class DebugIterator[X](xs: Iterator[X]) extends Iterator[X] {
  def hasNext: Boolean = xs.hasNext

  def next(): X = {
    val x = xs.next(); println(x); x
  }
}