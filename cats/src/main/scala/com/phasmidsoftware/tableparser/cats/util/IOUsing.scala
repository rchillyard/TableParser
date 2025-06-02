/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.util

import cats.effect.IO
import cats.effect.kernel.Resource
import scala.util.Using.Releasable
import scala.util._

/**
 * Cats effect IO equivalent of TryUsing
 */
object IOUsing {
  /**
   * This apply method is to allow proper handling of Releasable resources, using IO.
   *
   * @param resource a resource which is used by f and will be managed by Resource.
   * @param f        a function of R => IO[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a IO[A], the result of invoking apply(IO(resource))(f).
   */
  def apply[R: Releasable, A](resource: => R)(f: R => IO[A]): IO[A] = IOUsing(IO(resource))(f)

  /**
   * This alternative apply method signature is to allow proper handling of Releasable resources which are themselves wrapped in IO, using IO.
   * This method is to Using.apply as flatMap is to Map.
   *
   * CONSIDER making the ri parameter non-strict (but then we would have to rename this method something else).
   *
   * @param ri an IO of a resource which is to be used by f and will be managed by Resource.
   * @param f  a function of R => IO[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a IO[A]
   */
  def apply[R: Releasable, A](ri: IO[R])(f: R => IO[A]): IO[A] = Resource.make(ri)(r => IO(implicitly[Releasable[R]].release(r))).use(r => f(r))

  /**
   * This alternative apply method signature is to allow proper handling of Releasable resources which are themselves wrapped in Try, using IO.
   *
   * CONSIDER making the ry parameter non-strict (but then we would have to rename this method something else).
   *
   * @param ry a Try of a resource which is to be used by f and will be managed by Resource.
   * @param f  a function of R => IO[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a IO[A]
   */
  def apply[R: Releasable, A](ry: Try[R])(f: R => IO[A]): IO[A] = apply(IO.fromTry(ry))(f)
}
