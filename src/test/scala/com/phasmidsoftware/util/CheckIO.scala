package com.phasmidsoftware.util

import cats.effect.IO
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.time.{Second, Span}

/**
 * Interim utilities for checking IO.
 *
 * Once we move to cats version 3, we will be able to use https://github.com/typelevel/cats-effect-testing
 */
object CheckIO extends Futures with ScalaFutures {

  /**
   * Check the result by converting it to a Future, and waiting for it to complete.
   *
   * @param result an IO[X].
   * @param timeout a Timeout value (defaults to 1 second).
   * @param check a partial function of type X => Unit to invoke on the result.
   * @tparam X the underlying type of the result.
   */
  def checkResultIO[X](result: => IO[X], timeout: Timeout = Timeout(Span(1, Second)))(check: PartialFunction[X, Unit]): Unit = {
    import cats.effect.unsafe.implicits.global
    whenReady(result.unsafeToFuture(), timeout)(check(_))
  }

}
