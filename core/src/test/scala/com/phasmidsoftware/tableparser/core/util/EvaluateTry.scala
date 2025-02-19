package com.phasmidsoftware.tableparser.core.util

import org.scalatest.Assertion
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

/**
 * Interim utilities for checking Try.
 *
 * Once we move to cats version 3, we will be able to use https://github.com/typelevel/cats-effect-testing
 */
object EvaluateTry extends Futures with ScalaFutures with should.Matchers {

  /**
   * Method to evaluate an Try[X] as an Assertion.
   * It does this by converting to a Future,
   * waiting for that Future to finish and
   * then passing the result to the given partialFunction.
   * For usage, see EvaluateIOSpec.
   *
   * @param xy              a value of Try[X].
   * @param partialFunction a PartialFunction of type X=>Assertion
   * @tparam X the underlying type of xy.
   * @return an Assertion.
   */
  def matchTry[X](xy: => Try[X])(partialFunction: PartialFunction[X, Assertion]): Assertion =
    xy match {
      case Success(x) => if (partialFunction.isDefinedAt(x)) partialFunction(x) else fail(s"$x did not match")
      case Failure(x) => fail(x.getLocalizedMessage)
    }
}
