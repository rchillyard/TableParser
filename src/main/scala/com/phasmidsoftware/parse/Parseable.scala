package com.phasmidsoftware.parse

import java.io.File
import java.net.URL

import org.joda.time.LocalDate

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Type class which describes a type which can be parsed from a String.
  *
  * @tparam T the resulting type.
  */
trait Parseable[T] {

  def parse(s: String): T

  // NOTE not currently used
  def tryParse(s: String): Try[T] = Try(parse(s))
}

object Parseable {

  trait ParseableBoolean extends Parseable[Boolean] {
    override def parse(s: String): Boolean = try s.toBoolean catch {
      case _: IllegalArgumentException => throw ParserException(s"ParseableBoolean: cannot interpret '$s' as a Boolean")
    }
  }

  implicit object ParseableBoolean extends ParseableBoolean

  trait ParseableInt extends Parseable[Int] {
    override def parse(s: String): Int = try s.toInt catch {
      case _: IllegalArgumentException => throw ParserException(s"ParseableInt: cannot interpret '$s' as an Int")
    }
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

  /**
    * This trait splits strings of the form {x,y,z}, regardless of the format specified by the RowConfig object.
    */
  trait ParseableStringList$ extends Parseable[StringList] {
    override def parse(s: String): StringList = split(s)
  }

  implicit object ParseableStringList$ extends ParseableStringList$

  private val parser = new ListParser()

  def split(w: String): StringList = parser.parseAll(parser.list, w) match {
    case parser.Success(ws: StringList, _) => ws
    case parser.Failure(msg, _) => throw ParserException(s"cannot split string '$w': $msg")
    case parser.Error(msg, _) => throw ParserException(s"cannot split string '$w': $msg")
    case _ => throw ParserException(s"cannot split string '$w'")
  }
}

abstract class ParseableOption[T: Parseable] extends Parseable[Option[T]] {
  override def parse(s: String): Option[T] = Try(implicitly[Parseable[T]].parse(s)).toOption
}

object ParseableOption {

  implicit object ParseableOptionBoolean extends ParseableOption[Boolean]

  implicit object ParseableOptionInt extends ParseableOption[Int]

  implicit object ParseableOptionDouble extends ParseableOption[Double]

  implicit object ParseableOptionLong extends ParseableOption[Long]

  implicit object ParseableOptionLocalDate extends ParseableOption[LocalDate]

  implicit object ParseableOptionURL extends ParseableOption[URL]

  implicit object ParseableOptionFile extends ParseableOption[File]

}


class ListParser() extends JavaTokenParsers {

  def list: Parser[StringList] = "{" ~> repsep("""[^\,\}]+""".r, ",") <~ "}"
}
