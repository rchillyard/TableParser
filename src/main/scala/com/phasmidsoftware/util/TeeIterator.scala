/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

/**
 * This iterator gets its input from a Iterator[X], which is essentially a parameterless function.
 * The first n elements of the iterator can be passed to the caller of tee.
 *
 * CONSIDER passing the parameter n to the tee method instead of the constructor.
 *
 * @param n  the number of elements to detach.
 * @param xs the input iterator.
 * @tparam X the underlying type.
 */
class TeeIterator[X](n: Int)(xs: Iterator[X]) extends Iterator[X] {
  // CONSIDER replacing xs.hasNext and xs.next with hasNext and next
  // CONSIDER making tee lazy.
  val tee: Seq[X] = for (_ <- 0 until n if xs.hasNext) yield xs.next()

  def hasNext: Boolean = xs.hasNext

  def next(): X = xs.next()
}
