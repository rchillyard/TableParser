/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.util

import com.phasmidsoftware.tableparser.core.parse.MultiLineException
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.util.{Failure, Success, Try}

/**
 * This iterator gets its input from a call-by-name value, which is essentially a parameterless function.
 *
 * @param f the call-by-name value.
 * @tparam X a Joinable input type.
 * @tparam R the underlying type of f and also the result.
 */
class FunctionIterator[X: Joinable, R](f: X => Try[R])(xs: Iterator[X]) extends AbstractIterator[Try[R]] {
  private val xj = implicitly[Joinable[X]]

  // NOTE: yes, this has to be a var.
  private var ry: Try[R] = Failure(new Exception("FunctionIterator: uninitialized"))

  def hasNext: Boolean = takeFromIterator match {
    case Success(r) =>
      ry = Success(r); true
    case Failure(IteratorExhaustedException) =>
      false // NOTE: normal end of iterator
    case Failure(f@MultiLineException(_)) =>
      throw f // NOTE: this would be a logic error
    case f@Failure(_) =>
      ry = f; true
  }

  def next(): Try[R] = ry

  private def takeFromIterator: Try[R] = {
    def invokeFunction(xy: Try[X]) = xy match {
      case Success(x) =>
        if (xj.valid(x))
          f(x)
        else
          Failure(InvalidInputException)
      case Failure(x) =>
        Failure(x)
    }

    @tailrec
    def inner(xy: Try[X]): Try[R] = invokeFunction(xy) match {
      case Success(y) =>
        Success(y)
      case Failure(MultiLineException(x: X)) =>
        if (xs.hasNext) {
          val x1 = xs.next()
          val x2: X = xj.join(x, x1)
          inner(Success(x2))
        }
        else
          Failure(IteratorExhaustedException)
      case Failure(InvalidInputException) =>
        Failure(IteratorExhaustedException)
      case f@Failure(_) =>
        f
    }

    inner(Failure(MultiLineException(xj.zero)))
  }

}

/**
 * `IteratorExhaustedException` is a case object that represents an exception to be thrown
 * when an attempt is made to access an element from an iterator that has been fully traversed.
 *
 * This exception helps signal that the iterator is exhausted, meaning no more elements
 * are available for consumption.
 *
 * @constructor Creates a new `IteratorExhaustedException` with a predefined message: "iterator exhausted".
 *
 *              Example usage:
 * {{{
 * val iterator = Iterator.empty
 * if (!iterator.hasNext) throw IteratorExhaustedException
 * }}}
 */
case object IteratorExhaustedException extends Exception("iterator exhausted")

/**
 * The `InvalidInputException` is a case object that extends the base `Exception` class.
 * It represents an exception that is thrown when an invalid input is encountered.
 *
 * @constructor This exception is initialized with a default error message: "invalid input".
 *
 *              Usage example:
 * {{{
 * if (input.isInvalid) {
 *   throw InvalidInputException
 * }
 * }}}
 *
 *              This exception can be used in scenarios where there is a need to signal
 *              that the provided input data is not valid or does not meet the required criteria.
 */
case object InvalidInputException extends Exception(s"invalid input")