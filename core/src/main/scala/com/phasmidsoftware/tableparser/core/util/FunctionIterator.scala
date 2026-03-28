/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.util

import com.phasmidsoftware.tableparser.core.parse.MultiLineException
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.util.{Failure, Success, Try}

/**
 * The `FunctionIterator` class wraps an existing iterator to apply a function `f` to its elements,
 * returning an iterator that yields `Try[R]`, where `R` is the result type of the function application.
 *
 * This class processes elements from a given iterator of type `X` and handles various scenarios such as:
 * - Validating inputs against a custom `Joinable` type class.
 * - Handling exceptions during function execution and input processing.
 * - Providing support for "multi-line" inputs that can be joined using the `Joinable` logic.
 *
 * @tparam X The input type of the iterator.
 *           Must have an implicit type class `Joinable` that provides methods for validation, joining, and a zero value.
 * @tparam R The result type produced by applying function `f` to elements of type `X`.
 * @param f  A function from `X` to `Try[R]` that is applied to each element of the iterator.
 * @param xs An `Iterator[X]` containing the input elements to be processed.
 *
 *           The class extends `AbstractIterator[Try[R]]` and provides these key methods:
 * @constructor
 * Creates a `FunctionIterator` that wraps the input iterator and applies the given function to its elements.
 *
 * Usage example:
 * {{{
 * implicit object StringJoinable extends Joinable[String] {
 *   def join(t1: String, t2: String): String = t1 + t2
 *   val zero: String = ""
 *   def valid(t: String): Boolean = t.nonEmpty
 * }
 *
 * val inputs = Iterator("abc", "123", "")
 * val function: String => Try[Int] = str => Try(str.toInt)
 * val functionIterator = new FunctionIterator(function)(inputs)
 *
 * while (functionIterator.hasNext) {
 *   println(functionIterator.next())
 * }
 * }}}
 * @note The `FunctionIterator` may use a mutable variable `ry` internally to store the result of the last element processed.
 */
class FunctionIterator[X: Joinable, R](f: X => Try[R])(xs: Iterator[X]) extends AbstractIterator[Try[R]] {
  private val xj = implicitly[Joinable[X]]

  // NOTE: yes, this has to be a var. Don't be too heartbroken--an Iterator is a mutable construct, after all.
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