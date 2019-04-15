/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import scala.util.Try

object FP {
  /**
    *
    * @param xys a sequence of Try[X]
    * @tparam X the underlying type
    * @return a Try of Seq[X]
    *         NOTE: that the output collection type will be Seq, regardless of the input type
    */
  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = (Try(Seq[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

}
