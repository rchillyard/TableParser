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

  /**
    * Parser of non-empty strings.
    * The exception is useful for ensuring a None in the case of an optional String.
    * CONSIDER possibly a better way to do it.
    */
  trait ParseableString extends Parseable[String] {

    override def parse(s: String): String = if (s.isEmpty) throw ParseableException("empty String") else s
  }

  implicit object ParseableString extends ParseableString

  trait ParseableBoolean extends Parseable[Boolean] {
    override def parse(s: String): Boolean = try s.toBoolean catch {
      case _: IllegalArgumentException => throw ParseableException(s"ParseableBoolean: cannot interpret '$s' as a Boolean")
    }
  }

  implicit object ParseableBoolean extends ParseableBoolean

  trait ParseableInt extends Parseable[Int] {
    override def parse(s: String): Int = try s.toInt catch {
      case _: IllegalArgumentException => throw ParseableException(s"ParseableInt: cannot interpret '$s' as an Int")
    }
  }

  implicit object ParseableInt extends ParseableInt

  trait ParseableLong extends Parseable[Long] {
    override def parse(s: String): Long = try s.toLong catch {
      case _: IllegalArgumentException => throw ParseableException(s"ParseableLong: cannot interpret '$s' as a Long")
    }
  }

  implicit object ParseableLong extends ParseableLong

  trait ParseableBigInt extends Parseable[BigInt] {
    override def parse(s: String): BigInt = try BigInt(s) catch {
      case _: IllegalArgumentException => throw ParseableException(s"ParseableBigInt: cannot interpret '$s' as a BigInt")
    }
  }

  implicit object ParseableBigInt extends ParseableBigInt

  trait ParseableDouble extends Parseable[Double] {
    override def parse(s: String): Double = try s.toDouble catch {
      case _: IllegalArgumentException => throw ParseableException(s"ParseableDouble: cannot interpret '$s' as a Double")
    }
  }

  implicit object ParseableDouble extends ParseableDouble

  trait ParseableLocalDate extends Parseable[LocalDate] {
    override def parse(s: String): LocalDate = try LocalDate.parse(s) catch {
      case e: IllegalArgumentException => throw ParseableException(s"ParseableLocalDate: cannot interpret '$s' as a LocalDate", e)
    }
  }

  implicit object ParseableLocalDate extends ParseableLocalDate

  trait ParseableURL extends Parseable[URL] {
    override def parse(s: String): URL = try new URL(s) catch {
      case e: IllegalArgumentException => throw ParseableException(s"ParseableURL: cannot interpret '$s' as an URL", e)
    }
  }

  implicit object ParseableURL extends ParseableURL

  trait ParseableFile extends Parseable[File] {
    override def parse(s: String): File = try new File(s) catch {
      case e: IllegalArgumentException => throw ParseableException(s"ParseableFile: cannot interpret '$s' as a File", e)
    }
  }

  implicit object ParseableFile extends ParseableFile

  /**
    * This trait splits strings of the form {x,y,z}, regardless of the format specified by the RowConfig object.
    */
  trait ParseableStringList extends Parseable[StringList] {
    override def parse(s: String): StringList = try split(s) catch {
      case e: IllegalArgumentException => throw ParseableException(s"ParseableStringList: cannot interpret '$s' as a List[String]", e)
    }
  }

  implicit object ParseableStringList extends ParseableStringList

  private val parser = new ListParser()

  def split(w: String): StringList = parser.parseAll(parser.list, w) match {
    case parser.Success(ws: StringList, _) => ws
    case parser.Failure(msg, _) => throw ParseableException(s"cannot split string '$w': $msg")
    case parser.Error(msg, _) => throw ParseableException(s"cannot split string '$w': $msg")
    case _ => throw ParseableException(s"cannot split string '$w'")
  }
}

abstract class ParseableOption[T: Parseable] extends Parseable[Option[T]] {
  override def parse(s: String): Option[T] = Try(implicitly[Parseable[T]].parse(s)).toOption
}

object ParseableOption {

  implicit object ParseableOptionString extends ParseableOption[String]

  implicit object ParseableOptionBoolean extends ParseableOption[Boolean]

  implicit object ParseableOptionInt extends ParseableOption[Int]

  implicit object ParseableOptionDouble extends ParseableOption[Double]

  implicit object ParseableOptionLong extends ParseableOption[Long]

  implicit object ParseableOptionLocalDate extends ParseableOption[LocalDate]

  implicit object ParseableOptionURL extends ParseableOption[URL]

  implicit object ParseableOptionFile extends ParseableOption[File]

}


class ListParser() extends JavaTokenParsers {

  def list: Parser[StringList] = "{" ~> strings <~ "}" | singleton

  def strings: Parser[StringList] = repsep("""[^\,\}]+""".r, ",")

  def singleton: Parser[StringList] = """\w*""".r ^^ { w: String => List(w) }
}

case class ParseableException(msg: String, e: Throwable = null) extends Exception(msg, e)
