package com.phasmidsoftware.tableparser.zio.util

import zio._
import zio.test._

/**
 * Interim utilities for checking IO.
 * TODO eliminate use of unsafe methods.
 * Once we move to cats version 3, we will be able to use https://github.com/typelevel/cats-effect-testing
 */
object EvaluateZIO {
//
//  /**
//   * Method to evaluate an IO[X] as an X.
//   * It does this by converting to a Future and then waiting for that Future to finish.
//   *
//   * @param xio     a value of IO[X].
//   * @param timeout a timeout (defaults to 1 second).
//   * @tparam X the underlying type of xio and the type of the result.
//   * @return an X.
//   */
//  def apply[X](xio: => Task[X], timeout: Timeout = Timeout(Span(1, Second))): X =
//    // TODO eliminate use of unsafe methods
//    whenReady(xio.toFuture(), timeout)(identity)

  /**
   * Method to evaluate an Task[X] as an Assertion.
   * It does this by converting to a Future,
   * waiting for that Future to finish and
   * then passing the result to the given partialFunction.
   * For usage, see EvaluateZIOSpec.
   *
   * @param xio             a value of Task[X].
   * @param timeout         a timeout (defaults to 1 second).
   * @param partialFunction a PartialFunction of type X=>Assertion
   * @tparam X the underlying type of xio.
   * @return an Assertion.
   */
  def matchIO[X](xio: => Task[X])(partialFunction: PartialFunction[X, Boolean]): Task[TestResult] = {
    val f: (=> X) => Boolean = partialFunction(_)
    assertZIO(xio)(Assertion.assertion("matchIO OK") {
      f
    })
  }
//
//  /**
//   * Check the result by converting it to a Future, and waiting for it to complete.
//   *
//   * CONSIDER returning Task[Assertion] like checkFailure.
//   *
//   * @param xio     an Task[X].
//   * @param timeout a Timeout value (defaults to 1 second).
//   * @param check   a partial function of type X => Unit to invoke on the result.
//   * @tparam X the underlying type of the result.
//   */
//  def check[X](xio: => Task[X])(check: PartialFunction[X, Unit]): Unit = {
//    // TODO eliminate use of unsafe methods
//    assertZIO(xio)(Assertion.fails(new Assertion[Throwable]{}))
//  }

//  /**
//   * Method to check that the result is a fail (throws an exception).
//   *
//   * NOTE: you MUST evaluate the Assertion returned wrapped in IO, otherwise any failure will not be noticed.
//   * For example, z.runSync()
//   *
//   * @param xio      an IO[X]
//   * @param expected the expected exception class.
//   * @tparam X the underlying type of the result.
//   * @return an IO[Assertion].
//   */
//  def checkFailure[Z, X: ClassTag](xio: => Task[Z], x: Throwable): Task[TestResult] = {
//    assertZIO(xio.exit)(fails(isSubtype[Throwable](Assertion.assertion("matchIO OK") { z => z == implicitly[ClassTag[X]] })))
//
//    val result: ZIO[Any, Nothing, ZIO[Any, Throwable, TestResult]] = for (z <- xio.exit) yield assertZIO(z)(fails(equalTo(x)))
//    result.
//  }
}
