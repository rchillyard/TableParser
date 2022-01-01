/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

/**
 * This iterator gets its input from a call-by-name value, which is essentially a parameterless function.
 *
 * @param n the number of elements to detach.
 * @tparam X the underlying type.
 */
class TeeIterator[X](n: Int)(xs: Iterator[X]) extends Iterator[X] {
  val tee: Seq[X] = for (_ <- 0 until n if xs.hasNext) yield xs.next()

  def hasNext: Boolean = xs.hasNext

  def next(): X = xs.next()
}
