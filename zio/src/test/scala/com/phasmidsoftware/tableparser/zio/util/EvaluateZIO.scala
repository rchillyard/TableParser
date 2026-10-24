package com.phasmidsoftware.tableparser.zio.util

import zio._

/**
 * Interim utilities for checking IO.
 * TODO eliminate use of unsafe methods.
 * Once we move to cats version 3, we will be able to use https://github.com/typelevel/cats-effect-testing
 */
object EvaluateZIO:

  /**
   * Method to run a Task[X] (unsafely, so only in tests) and return its outcome.
   *
   * @param xio a Task[X].
   * @tparam X the underlying type of xio.
   * @return Right(x) if it succeeded, otherwise Left(the exception, whether a failure or a defect).
   */
  def run[X](xio: => Task[X]): Either[Throwable, X] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(xio) match
        case Exit.Success(x) => Right(x)
        case Exit.Failure(cause) => Left(cause.squash)
    }
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
   * Method to run a Task[X] and check its result with a partial function, which must be defined for it and yield true.
   * The Task is run (unsafely, so only in tests).
   * NOTE: up to 2.0.2, this returned a Task[TestResult], which the tests never ran, so they checked nothing.
   * For usage, see EvaluateZIOSpec.
   *
   * @param xio             a value of Task[X].
   * @param partialFunction a PartialFunction of type X=>Boolean.
   * @tparam X the underlying type of xio.
   * @return an Assertion.
   */
  def matchZIO[X](xio: => Task[X])(partialFunction: PartialFunction[X, Boolean]): org.scalatest.Assertion =
    import org.scalatest.Assertions.{assert, fail}
    run(xio) match
      case Right(x) if partialFunction.isDefinedAt(x) => assert(partialFunction(x), s"matchZIO: the check was false for $x")
      case Right(x) => fail(s"matchZIO: no case matched $x")
      case Left(e) => fail(s"matchZIO: the Task failed with $e", e)
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
//    assertZIO(xio.exit)(fails(isSubtype[Throwable](Assertion.assertion("matchZIO OK") { z => z == summon[ClassTag[X]] })))
//
//    val result: ZIO[Any, Nothing, ZIO[Any, Throwable, TestResult]] = for (z <- xio.exit) yield assertZIO(z)(fails(equalTo(x)))
//    result.
//  }
