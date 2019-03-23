package com.phasmidsoftware.parse

import java.io.File
import java.net.URL

import org.joda.time.LocalDate

import scala.util.Try

/**
  * Type class which describes a type which can be parsed from a String.
  *
  * @tparam T the resulting type.
  */
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

  trait ParseableLocalDate extends Parseable[LocalDate] {
    override def parse(s: String): LocalDate = LocalDate.parse(s)
  }

  implicit object ParseableLocalDate extends ParseableLocalDate

  trait ParseableURL extends Parseable[URL] {
    override def parse(s: String): URL = new URL(s)
  }

  implicit object ParseableURL extends ParseableURL

  trait ParseableFile extends Parseable[File] {
    override def parse(s: String): File = new File(s)
  }

  implicit object ParseableFile extends ParseableFile

}
