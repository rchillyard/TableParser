package com.phasmidsoftware.format

import scala.util.Try

trait Parseable[T] {

  def parse(s: String): T

  def tryParse(s: String): Try[T] = Try(parse(s))
}

object Parseable {

  trait ParseableBoolean extends Parseable[Boolean] {
    override def parse(s: String): Boolean = s.toBoolean
  }

  implicit object ParseableBoolean extends ParseableBoolean

  trait ParseableInt extends Parseable[Int] {
    override def parse(s: String): Int = s.toInt
  }

  implicit object ParseableInt extends ParseableInt

  trait ParseableLong extends Parseable[Long] {
    override def parse(s: String): Long = s.toLong
  }

  implicit object ParseableLong extends ParseableLong

  trait ParseableDouble extends Parseable[Double] {
    override def parse(s: String): Double = s.toDouble
  }

  implicit object ParseableDouble extends ParseableDouble

}
