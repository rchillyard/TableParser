/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.util

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
  /**
   * A sequence that contains the first `n` elements from the underlying iterator. This is
   * achieved by iterating up to `n` times (or until the iterator has no more elements).
   *
   * Elements are fetched using the `next()` method only if the `hasNext` method indicates that
   * there are more elements available in the iterator.
   *
   * @note This eagerly evaluates and retrieves `n` or fewer elements from the underlying iterator
   *       upon initialization.
   * @see [[hasNext]] to check if more elements are available.
   * @see [[next]] to retrieve the next element.
   */
  val tee: Seq[X] =
    for (_ <- 0 until n if hasNext) yield next()

  /**
   * Checks if the underlying iterator has more elements.
   *
   * @return true if the underlying iterator has more elements, false otherwise.
   */
  def hasNext: Boolean =
    xs.hasNext

  /**
   * Retrieves the next element in the iterator.
   *
   * @return the next element of type `X` from the underlying iterator.
   */
  def next(): X =
    xs.next()
}

/**
 * TESTME
 *
 * @param xs an iterator on X.
 * @tparam X the underlying type of the iterator.
 */
class DebugIterator[X](xs: Iterator[X]) extends Iterator[X] {
  /**
   * Checks if there are more elements available in the iterator.
   *
   * @return `true` if there are more elements to iterate over, `false` otherwise.
   */
  def hasNext: Boolean =
    xs.hasNext

  /**
   * Retrieves the next element from the underlying iterator and prints it to the console.
   *
   * @return the next element of type X from the iterator.
   */
  def next(): X = {
    val x = xs.next()
    println(x)
    x
  }
}