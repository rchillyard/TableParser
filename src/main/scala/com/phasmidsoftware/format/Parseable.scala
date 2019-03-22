package com.phasmidsoftware.format

import org.joda.time.LocalDate
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.util.Try

trait Parseable[T] {

  def parse(s: String): T

  def tryParse(s: String): Try[T] = Try(parse(s))
}

object Parseable {

  trait ParseableInt extends Parseable[Int] {
    override def parse(s: String): Int = s.toInt
  }

  implicit object ParseableInt extends ParseableInt

  //  import com.github.nscala_time.time.Imports._
  trait ParseableLocalDate extends Parseable[LocalDate] {

    //    import org.joda.time.format.DateTimeFormat
    //
    //    import org.joda.time.format.DateTimeFormatter

    val fmt: DateTimeFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

    override def parse(s: String): LocalDate = LocalDate.parse(s, fmt)
  }

  implicit object ParseableLocalDate extends ParseableLocalDate

}
