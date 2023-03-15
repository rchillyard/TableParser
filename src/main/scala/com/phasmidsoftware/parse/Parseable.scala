/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import java.io.File
import java.net.URL
import org.joda.time.LocalDate
import scala.annotation.implicitNotFound
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

/**
 * Type class which describes a type which can be parsed from a String.
 *
 * @tparam T the resulting type.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of Parseable[${T}]. This is unusual when your application types are all case classes. Most of the standard types are supported in the Parseable companion object. Take a look and define something similar that works for your type, or consider redefining your type as a case class.")
trait Parseable[T] {

  /**
   * Parse a String as a Try[T].
   *
   * @param s the String to be parsed.
   * @return the corresponding value of type T, wrapped in Try.
   */
  def parse(s: String, optModifier: Option[String] = None): Try[T]
}

object Parseable {

  /**
   * Parser of String.
   * The exception is useful for ensuring a None in the case of an optional String.
   */
  trait ParseableString extends Parseable[String] {
    def parse(s: String, optModifier: Option[String]): Try[String] = if (s.isEmpty) Failure(BlankException()) else Success(s)
  }

  implicit object ParseableString extends ParseableString

  /**
   * Parser of Boolean.
   */
  trait ParseableBoolean extends Parseable[Boolean] {
    def parse(s: String, optModifier: Option[String]): Try[Boolean] = parseAndRecover(s)(lift(_.toBoolean))(w => s"ParseableBoolean: cannot interpret '$w' as a Boolean")
  }

  implicit object ParseableBoolean extends ParseableBoolean

  /**
   * Parser of Byte.
   */
  trait ParseableByte extends Parseable[Byte] {
    def parse(s: String, optModifier: Option[String]): Try[Byte] = parseAndRecover(s)(lift(_.toByte))(w => s"ParseableByte: cannot interpret '$w' as a Byte")
  }

  implicit object ParseableByte extends ParseableByte

  /**
   * Parser of Char.
   */
  trait ParseableChar extends Parseable[Char] {
    def parse(s: String, optModifier: Option[String]): Try[Char] = parseAndRecover(s)(lift(_.head))(w => s"ParseableChar: cannot interpret '$w' as a Char")
  }

  implicit object ParseableChar extends ParseableChar

  /**
   * Parser of Short.
   */
  trait ParseableShort extends Parseable[Short] {
    def parse(s: String, optModifier: Option[String]): Try[Short] = parseAndRecover(s)(lift(_.toShort))(w => s"ParseableShort: cannot interpret '$w' as a Short")
  }

  implicit object ParseableShort extends ParseableShort

  /**
   * Parser of Int.
   */
  trait ParseableInt extends Parseable[Int] {
    // CONSIDER supporting different radix
    def parse(s: String, optModifier: Option[String]): Try[Int] = parseAndRecover(s)(lift(_.toInt))(w => s"ParseableInt: cannot interpret '$w' as an Int")
  }

  implicit object ParseableInt extends ParseableInt

  /**
   * Parser of Long.
   */
  trait ParseableLong extends Parseable[Long] {
    // CONSIDER supporting different radix
    def parse(s: String, optModifier: Option[String]): Try[Long] = parseAndRecover(s)(lift(_.toLong))(w => s"ParseableLong: cannot interpret '$w' as a Long")
  }

  implicit object ParseableLong extends ParseableLong

  /**
   * Parser of BigInt.
   */
  trait ParseableBigInt extends Parseable[BigInt] {
    def parse(s: String, optModifier: Option[String]): Try[BigInt] = {
      val radix = optModifier map (_.toInt) getOrElse 10
      val f: String => BigInt = BigInt(_, radix)
      parseAndRecover(s)(lift(f))(w => s"ParseableBigInt: cannot interpret '$w' in radix $radix as a BigInt")
    }
  }

  implicit object ParseableBigInt extends ParseableBigInt

  /**
   * Parser of BigDecimal.
   */
  trait ParseableBigDecimal extends Parseable[BigDecimal] {
    def parse(s: String, optModifier: Option[String]): Try[BigDecimal] = parseAndRecover(s)(lift(BigDecimal.apply))(w => s"ParseableBigDecimal: cannot interpret '$w' as a BigDecimal")
  }

  implicit object ParseableBigDecimal extends ParseableBigDecimal

  /**
   * Parser of Double.
   */
  trait ParseableDouble extends Parseable[Double] {
    def parse(s: String, optModifier: Option[String]): Try[Double] = parseAndRecover(s)(lift(_.toDouble))(w => s"ParseableDouble: cannot interpret '$w' as a Double")
  }

  implicit object ParseableDouble extends ParseableDouble

  /**
   * Parser of Float.
   */
  trait ParseableFloat extends Parseable[Float] {
    def parse(s: String, optModifier: Option[String]): Try[Float] = parseAndRecover(s)(lift(_.toFloat))(w => s"ParseableFloat: cannot interpret '$w' as a Float")
  }

  implicit object ParseableFloat extends ParseableFloat

  /**
   * Parser of LocalDate.
   */
  trait ParseableLocalDate extends Parseable[LocalDate] {
    def parse(s: String, optModifier: Option[String]): Try[LocalDate] = parseAndRecover(s)(lift(LocalDate.parse))(w => s"ParseableLocalDate: cannot interpret '$w' as a LocalDate")
  }

  implicit object ParseableLocalDate extends ParseableLocalDate

  /**
   * Parser of URL.
   */
  trait ParseableURL extends Parseable[URL] {
    def parse(s: String, optModifier: Option[String]): Try[URL] = parseAndRecover(s)(lift(new URL(_)))(w => s"ParseableURL: cannot interpret '$w' as an URL")
  }

  implicit object ParseableURL extends ParseableURL

  /**
   * Parser of File.
   */
  trait ParseableFile extends Parseable[File] {
    def parse(s: String, optModifier: Option[String]): Try[File] = parseAndRecover(s)(lift(new File(_)))(w => s"ParseableFile: cannot interpret '$w' as a File")
  }

  implicit object ParseableFile extends ParseableFile

  /**
   * Parser of StringList.
   * This trait splits strings of the form {x,y,z}, regardless of the format specified by the RowConfig object.
   */
  trait ParseableStringList extends Parseable[StringList] {
    def parse(s: String, optModifier: Option[String]): Try[StringList] = parseAndRecover(s)(split)(w => s"ParseableStringList: cannot interpret '$w' as a StringList")
  }

  implicit object ParseableStringList extends ParseableStringList

  /**
   * Method to split a String into a StringList, parser.list.
   *
   * @param w the String to parse.
   * @return a Try[StringList].
   */
  def split(w: String): Try[StringList] = ListParser.parseAll(ListParser.list, w) match {
    case ListParser.Success(ws: StringList, _) => Success(ws)
    case ListParser.Failure(msg, _) => Failure(ParseLogicException(s"cannot split string '$w': $msg"))
    case ListParser.Error(msg, _) => Failure(ParseLogicException(s"cannot split string '$w': $msg"))
    case _ => Failure(ParseLogicException(s"cannot split string '$w'"))
  }

  private def lift[T](f: String => T): String => Try[T] = w => Try(f(w))

  private def parseAndRecover[T](w: String)(f: String => Try[T])(msg: String => String): Try[T] =
    f(w).recoverWith {
      case x: IllegalArgumentException =>
        Failure(if (w.nonEmpty) InvalidParseException(msg(w), x) else BlankException(x))
      case x: NumberFormatException =>
        Failure(if (w.nonEmpty) InvalidParseException(msg(w), x) else BlankException(x))
    }
}

/**
 * Abstract class to parse optional scala values.
 *
 * @tparam T the resulting type for which there must be evidence of a Parseable[T].
 */
abstract class ParseableOption[T: Parseable] extends Parseable[Option[T]] {
  def parse(s: String, optModifier: Option[String]): Try[Option[T]] = implicitly[Parseable[T]].parse(s, optModifier).map(Option(_)).recoverWith {
    case _: BlankException => Success(None)
  }
}

object ParseableOption {

  implicit object ParseableOptionString extends ParseableOption[String]

  implicit object ParseableOptionBoolean extends ParseableOption[Boolean]

  implicit object ParseableOptionByte extends ParseableOption[Byte]

  implicit object ParseableOptionShort extends ParseableOption[Short]

  implicit object ParseableOptionInt extends ParseableOption[Int]

  implicit object ParseableOptionFloat extends ParseableOption[Float]

  implicit object ParseableOptionDouble extends ParseableOption[Double]

  implicit object ParseableOptionLong extends ParseableOption[Long]

  implicit object ParseableOptionBigInt extends ParseableOption[BigInt]

  implicit object ParseableOptionBigDecimal extends ParseableOption[BigDecimal]

  implicit object ParseableOptionLocalDate extends ParseableOption[LocalDate]

  implicit object ParseableOptionURL extends ParseableOption[URL]

  implicit object ParseableOptionFile extends ParseableOption[File]

}

/**
 * This object is a parser of lists.
 * A list is considered to be enclosed by {} and separated by commas.
 */
object ListParser extends JavaTokenParsers {

  lazy val list: Parser[StringList] = "{" ~> strings <~ "}" | singleton

  lazy val strings: Parser[StringList] = repsep("""[^,}]+""".r, ",")

  lazy val singleton: Parser[StringList] = """\w*""".r ^^ { w: String => List(w) }
}

case class ParseLogicException(msg: String, e: Throwable = null) extends Exception(msg, e)

abstract class ParseableException(msg: String, e: Throwable = null) extends Exception(msg, e)

case class InvalidParseException(msg: String, e: Throwable = null) extends ParseableException(msg, e)

case class BlankException(e: Throwable = null) extends ParseableException("blank", e)
