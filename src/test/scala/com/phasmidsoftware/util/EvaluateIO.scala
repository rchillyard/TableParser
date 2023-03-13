package com.phasmidsoftware.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.Assertion
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.matchers.should
import org.scalatest.time.{Second, Span}
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Interim utilities for checking IO.
 *
 * Once we move to cats version 3, we will be able to use https://github.com/typelevel/cats-effect-testing
 */
object EvaluateIO extends Futures with ScalaFutures with should.Matchers {

  /**
   * Method to evaluate an IO[X] as an X.
   * It does this by converting to a Future and then waiting for that Future to finish.
   *
   * @param xio     a value of IO[X].
   * @param timeout a timeout (defaults to 1 second).
   * @tparam X the underlying type of xio and the type of the result.
   * @return an X.
   */
  def apply[X](xio: => IO[X], timeout: Timeout = Timeout(Span(1, Second))): X = whenReady(xio.unsafeToFuture(), timeout)(identity)

  /**
   * Method to evaluate an IO[X] as an Assertion.
   * It does this by converting to a Future,
   * waiting for that Future to finish and
   * then passing the result to the given partialFunction.
   * For usage, see EvaluateIOSpec.
   *
   * @param xio             a value of IO[X].
   * @param timeout         a timeout (defaults to 1 second).
   * @param partialFunction a PartialFunction of type X=>Assertion
   * @tparam X the underlying type of xio.
   * @return an Assertion.
   */
  def matcher[X](xio: => IO[X], timeout: Timeout = Timeout(Span(1, Second)))(partialFunction: PartialFunction[X, Assertion]): Assertion =
    whenReady(xio.unsafeToFuture(), timeout)(partialFunction)

  /**
   * Check the result by converting it to a Future, and waiting for it to complete.
   *
   * CONSIDER returning IO[Assertion] like checkFailure.
   *
   * @param xio     an IO[X].
   * @param timeout a Timeout value (defaults to 1 second).
   * @param check   a partial function of type X => Unit to invoke on the result.
   * @tparam X the underlying type of the result.
   */
  def check[X](xio: => IO[X], timeout: Timeout = Timeout(Span(1, Second)))(check: PartialFunction[X, Unit]): Unit = {
    whenReady(xio.unsafeToFuture(), timeout)(check(_))
  }

  /**
   * Method to check that the result is a fail (throws an exception).
   *
   * NOTE: you MUST evaluate the Assertion returned wrapped in IO, otherwise any failure will not be noticed.
   * For example, z.runSync()
   *
   * @param xio      an IO[X]
   * @param expected the expected exception class.
   * @tparam X the underlying type of the result.
   * @return an IO[Assertion].
   */
  def checkFailure[X](xio: => IO[X])(expected: Class[_]): IO[Assertion] = {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val af: Future[Assertion] = xio.unsafeToFuture().transform(xy => xy match {
      case Success(_) => Success(fail("should fail"))
      case Failure(x) => if (expected.isAssignableFrom(x.getClass)) Success(succeed) else Success(fail(s"$x is not a $expected"))
    })
    IO.fromFuture(IO(af))
  }
}
