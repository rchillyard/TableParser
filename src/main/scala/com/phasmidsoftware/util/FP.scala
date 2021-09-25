/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import java.net.URL
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}

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
    * @param xos an Iterable of Option[X]
    * @tparam X the underlying type
    * @return an Option of Seq[X]
    *         NOTE: that the output collection type will be Seq, regardless of the input type
    */
  def sequence[X](xos: Iterable[Option[X]]): Option[Seq[X]] =
    xos.foldLeft(Option(Seq[X]())) {
      (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
    }

  /**
    * Method to partition an  method to combine elements of Try.
    *
    * @param xys an Iterator of Try[X]
    * @tparam X the underlying type
    * @return a tuple of two iterators of Try[X], the first one being successes, the second one being failures.
    */
  def partition[X](xys: Iterator[Try[X]]): (Iterator[Try[X]], Iterator[Try[X]]) = xys.partition(_.isSuccess)

  /**
    * Method to partition an  method to combine elements of Try.
    *
    * @param xys a Seq of Try[X]
    * @tparam X the underlying type
    * @return a tuple of two Seqs of Try[X], the first one being successes, the second one being failures.
    */
  def partition[X](xys: Seq[Try[X]]): (Seq[Try[X]], Seq[Try[X]]) = xys.partition(_.isSuccess)

  /**
    * Method to yield a Try[URL] for a resource name and a given class.
    *
    * @param resourceName the name of the resource.
    * @param clazz        the class, relative to which, the resource can be found.
    * @return a Try[URL]
    */
  def getURLForResource(resourceName: String, clazz: Class[_] = getClass): Try[URL] = Option(clazz.getResource(resourceName)) match {
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
    case _ => Failure(FPException(s"Header column $w not found"))
  }
}

object TryUsing {
  /**
    * This method is to Using.apply as flatMap is to Map.
    *
    * @param resource a resource which is used by f and will be managed via Using.resource
    * @param f        a function of R => Try[A].
    * @tparam R the resource type.
    * @tparam A the underlying type of the result.
    * @return a Try[A]
    */
  def apply[R: Releasable, A](resource: => R)(f: R => Try[A]): Try[A] = Using(resource)(f).flatten
}

case class FPException(msg: String, eo: Option[Throwable] = None) extends Exception(msg, eo.orNull)
