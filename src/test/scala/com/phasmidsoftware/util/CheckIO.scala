package com.phasmidsoftware.util

import cats.effect.IO
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
object CheckIO extends Futures with ScalaFutures with should.Matchers {

  /**
    * Check the result by converting it to a Future, and waiting for it to complete.
    *
    * CONSIDER returning IO[Assertion] like checkFailureIO.
    *
    * @param result  an IO[X].
    * @param timeout a Timeout value (defaults to 1 second).
    * @param check   a partial function of type X => Unit to invoke on the result.
    * @tparam X the underlying type of the result.
    */
  def checkResultIO[X](result: => IO[X], timeout: Timeout = Timeout(Span(1, Second)))(check: PartialFunction[X, Unit]): Unit = {
    import cats.effect.unsafe.implicits.global
    whenReady(result.unsafeToFuture(), timeout)(check(_))
  }

  /**
    * Method to check that the result is a fail (throws an exception).
    *
    * @param result   an IO[X]
    * @param expected the expected exception class.
    * @tparam X the underlying type of the result.
    * @return an IO[Assertion].
    */
  def checkFailureIO[X](result: => IO[X])(expected: Class[_]): IO[Assertion] = {
    import cats.effect.unsafe.implicits.global
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val af: Future[Assertion] = result.unsafeToFuture().transform(xy => xy match {
      case Success(_) => Success(fail("should fail"))
      case Failure(x) =>
        if (expected.isAssignableFrom(x.getClass)) Success(succeed) else Success(fail(s"$x is not a $expected"))
    })
    IO.fromFuture(IO(af))
  }

}
