/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import cats.effect.{IO, Resource}
import java.net.URL
import scala.reflect.ClassTag
import scala.util.Using.Releasable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try, Using}

/**
 * Various utilities for functional programming.
 */
object FP {

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys an Iterator of Try[X]
   * @tparam X the underlying type
   * @return a Try of Iterator[X]
   */
  def sequence[X](xys: Iterator[Try[X]]): Try[Iterator[X]] = sequence(xys.to(List)).map(_.iterator)

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xos an Iterator of Try[X]
   * @tparam X the underlying type
   * @return a Try of Iterator[X]
   */
  def sequence[X](xos: Iterator[Option[X]]): Option[Iterator[X]] = sequence(xos.to(List)).map(_.iterator)

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys an Iterable of Try[X]
   * @tparam X the underlying type
   * @return a Try of Seq[X]
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequence[X](xys: Iterable[Try[X]]): Try[Seq[X]] =
    xys.foldLeft(Try(Seq[X]())) {
      (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys an Iterable of Try[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequenceForgiving[X](xys: Iterable[Try[X]]): Try[Seq[X]] =
    sequenceForgivingWith[X](xys) {
      case NonFatal(x) => System.err.println(s"forgiving: $x"); Success(None)
      case x => Failure(x)
    }

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys       an Iterable of Try[X].
   * @param pfFailure a partial function of type Throwable => Try of Option[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequenceForgivingWith[X](xys: Iterable[Try[X]])(pfFailure: PartialFunction[Throwable, Try[Option[X]]]): Try[Seq[X]] =
    sequenceForgivingTransform[X](xys)(x => Success(Some(x)), pfFailure)

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xys       an Iterable of Try[X].
   * @param fSuccess  a function of type X => Try of Option[X].
   * @param pfFailure a partial function of type Throwable => Try of Option[X].
   * @tparam X the underlying type.
   * @return a Try of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequenceForgivingTransform[X](xys: Iterable[Try[X]])(fSuccess: X => Try[Option[X]], pfFailure: PartialFunction[Throwable, Try[Option[X]]]): Try[Seq[X]] = {
    val xosy: Try[Seq[Option[X]]] = sequence(for (xy <- xys) yield xy.transform[Option[X]](fSuccess, pfFailure))
    for (xos <- xosy) yield xos.filter(_.isDefined).map(_.get)
  }

  /**
   * Sequence method to combine elements of Try.
   *
   * @param xos an Iterable of Option[X].
   * @tparam X the underlying type.
   * @return an Option of Seq[X].
   *         NOTE: that the output collection type will be Seq, regardless of the input type
   */
  def sequence[X](xos: Iterable[Option[X]]): Option[Seq[X]] =
    xos.foldLeft(Option(Seq[X]())) {
      (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
    }

  /**
   * Method to partition an  method to combine elements of Try as an Iterator.
   *
   * @param xys an Iterator of Try[X].
   * @tparam X the underlying type.
   * @return a tuple of two iterators of Try[X], the first one being successes, the second one being failures.
   */
  def partition[X](xys: Iterator[Try[X]]): (Iterator[Try[X]], Iterator[Try[X]]) = xys.partition(_.isSuccess)

  /**
   * Method to partition an  method to combine elements of Try.
   *
   * @param xys a Seq of Try[X].
   * @tparam X the underlying type.
   * @return a tuple of two Seqs of Try[X], the first one being successes, the second one being failures.
   */
  def partition[X](xys: Iterable[Try[X]]): (Iterable[Try[X]], Iterable[Try[X]]) = xys.partition(_.isSuccess)

  /**
   * Method to yield a URL for a given resourceForClass in the classpath for C.
   *
   * @param resourceName the name of the resourceForClass.
   * @tparam C a class of the package containing the resourceForClass.
   * @return a Try[URL].
   */
  def resource[C: ClassTag](resourceName: String): Try[URL] = resourceForClass(resourceName, implicitly[ClassTag[C]].runtimeClass)

  /**
   * Method to yield a Try[URL] for a resource name and a given class.
   *
   * @param resourceName the name of the resource.
   * @param clazz        the class, relative to which, the resource can be found (defaults to the caller's class).
   * @return a Try[URL]
   */
  def resourceForClass(resourceName: String, clazz: Class[_] = getClass): Try[URL] = Option(clazz.getResource(resourceName)) match {
    case Some(u) => Success(u)
    case None => Failure(FPException(s"$resourceName is not a valid resource for $clazz"))
  }

  /**
   * Method to determine if the String w was found at a valid index (i).
   *
   * @param w the String (ignored unless there's an exception).
   * @param i the index found.
   * @return Success(i) if all well, else Failure(exception).
   */
  def indexFound(w: String, i: Int): Try[Int] = i match {
    case x if x >= 0 => Success(x)
    case _ => Failure(FPException(s"Header column '$w' not found"))
  }

  /**
   * Method to transform a a Try[X] into an Option[X].
   * But, unlike "toOption," a Failure can be logged.
   *
   * @param f  a function to process any Exception (typically a logging function).
   * @param xy the input Try[X}.
   * @tparam X the underlying type.
   * @return an Option[X].
   */
  def tryToOption[X](f: Throwable => Unit)(xy: Try[X]): Option[X] = xy match {
    case Success(x) => Some(x)
    case Failure(NonFatal(x)) => f(x); None
    case Failure(x) => throw x
  }
}

object TryUsing {
  /**
   * This method is to Using.apply as flatMap is to Map.
   *
   * @param resource a resource which is used by f and will be managed via Using.apply
   * @param f        a function of R => Try[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a Try[A]
   */
  def apply[R: Releasable, A](resource: => R)(f: R => Try[A]): Try[A] = Using(resource)(f).flatten

  /**
   * This method is similar to apply(r) but it takes a Try[R] as its parameter.
   * The definition of f is the same as in the other apply, however.
   *
   * @param ry a Try[R] which is passed into f and will be managed via Using.apply
   * @param f  a function of R => Try[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a Try[A]
   */
  def apply[R: Releasable, A](ry: Try[R])(f: R => Try[A]): Try[A] = for (r <- ry; a <- apply(r)(f)) yield a
}

/**
 * Cats effect IO equivalent of TryUsing
 */
object IOUsing {
  /**
   * This method is to allow proper handling of Releasable resources, using IO.
   *
   * @param resource a resource which is used by f and will be managed by Resource.
   * @param f        a function of R => IO[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a IO[A], the result of invoking apply(IO(resource))(f).
   */
  def apply[R: Releasable, A](resource: => R)(f: R => IO[A]): IO[A] = apply(IO(resource))(f)

  /**
   * This method is to Using.apply as flatMap is to Map.
   *
   * @param ri an IO of a resource which is to be used by f and will be managed by Resource.
   * @param f  a function of R => IO[A].
   * @tparam R the resource type.
   * @tparam A the underlying type of the result.
   * @return a IO[A]
   */
  def apply[R: Releasable, A](ri: IO[R])(f: R => IO[A]): IO[A] = Resource.make(ri)(src => IO(implicitly[Releasable[R]].release(src))).use(src => f(src))
}

case class FPException(msg: String, eo: Option[Throwable] = None) extends Exception(msg, eo.orNull)
