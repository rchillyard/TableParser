/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import java.net.URL

import scala.util.{Failure, Success, Try}

object FP {
  /**
    *
    * @param xys a sequence of Try[X]
    * @tparam X the underlying type
    * @return a Try of Seq[X]
    *         NOTE: that the output collection type will be Seq, regardless of the input type
    */
  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
    * Method to yield a Try[URL] for a resource name and a given class.
    *
    * @param resourceName the name of the resource.
    * @param clazz the class, relative to which, the resource can be found.
    * @return a Try[URL]
    */
  def getURLforResource(resourceName: String, clazz: Class[_] = getClass): Try[URL] = Option(clazz.getResource(resourceName)) match {
    case Some(u) => Success(u)
    case None => Failure(TableParserException(s"$resourceName is not a valid resource for $clazz"))
  }

  def indexFound(w: String, i: Int): Try[Int] = i match {
    case x if x >= 0 => Success(x)
    case _ => Failure(TableParserException(s"Header column $w not found"))
  }


}

case class TableParserException(msg: String, e: Throwable = null) extends Exception(msg, e)