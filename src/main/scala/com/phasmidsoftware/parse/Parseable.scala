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
  def parse(s: String): Try[T]
}

object Parseable {

  /**
    * Parser of non-empty strings.
    * The exception is useful for ensuring a None in the case of an optional String.
    */
  trait ParseableString extends Parseable[String] {

    override def parse(s: String): Try[String] = if (s.isEmpty) Failure(ParseableException("empty String")) else Success(s)
  }

  implicit object ParseableString extends ParseableString

  trait ParseableBoolean extends Parseable[Boolean] {

    override def parse(s: String): Try[Boolean] = parseAndRecover(s)(_.toBoolean)(w => s"ParseableBoolean: cannot interpret '$w' as a Boolean")
  }

  implicit object ParseableBoolean extends ParseableBoolean

  trait ParseableInt extends Parseable[Int] {
    override def parse(s: String): Try[Int] = parseAndRecover(s)(_.toInt)(w => s"ParseableInt: cannot interpret '$w' as an Int")
  }

  implicit object ParseableInt extends ParseableInt

  trait ParseableShort extends Parseable[Short] {
    override def parse(s: String): Try[Short] = parseAndRecover(s)(_.toShort)(w => s"ParseableShort: cannot interpret '$w' as a Short")
  }

  implicit object ParseableShort extends ParseableShort

  trait ParseableByte extends Parseable[Byte] {
    override def parse(s: String): Try[Byte] = parseAndRecover(s)(_.toByte)(w => s"ParseableByte: cannot interpret '$w' as a Byte")
  }

  implicit object ParseableByte extends ParseableByte

  trait ParseableLong extends Parseable[Long] {
    override def parse(s: String): Try[Long] = parseAndRecover(s)(_.toLong)(w => s"ParseableLong: cannot interpret '$w' as a Long")
  }

  implicit object ParseableLong extends ParseableLong

  trait ParseableBigInt extends Parseable[BigInt] {
    override def parse(s: String): Try[BigInt] = parseAndRecover(s)(BigInt.apply)(w => s"ParseableBigInt: cannot interpret '$w' as a BigInt")
  }

  implicit object ParseableBigInt extends ParseableBigInt

  trait ParseableBigDecimal extends Parseable[BigDecimal] {
    override def parse(s: String): Try[BigDecimal] = parseAndRecover(s)(BigDecimal.apply)(w => s"ParseableBigDecimal: cannot interpret '$w' as a BigDecimal")
  }

  implicit object ParseableBigDecimal extends ParseableBigDecimal

  trait ParseableDouble extends Parseable[Double] {
    override def parse(s: String): Try[Double] = parseAndRecover(s)(_.toDouble)(w => s"ParseableDouble: cannot interpret '$w' as a Double")
  }

  implicit object ParseableDouble extends ParseableDouble

  trait ParseableLocalDate extends Parseable[LocalDate] {
    override def parse(s: String): Try[LocalDate] = parseAndRecover(s)(LocalDate.parse)(w => s"ParseableLocalDate: cannot interpret '$w' as a LocalDate")
  }

  trait ParseableFloat extends Parseable[Float] {
    override def parse(s: String): Try[Float] = parseAndRecover(s)(_.toFloat)(w => s"ParseableFloat: cannot interpret '$w' as a Float")
  }

  implicit object ParseableFloat extends ParseableFloat

  implicit object ParseableLocalDate extends ParseableLocalDate

  trait ParseableURL extends Parseable[URL] {
    override def parse(s: String): Try[URL] = parseAndRecover(s)(new URL(_))(w => s"ParseableURL: cannot interpret '$w' as an URL")
  }

  implicit object ParseableURL extends ParseableURL

  trait ParseableFile extends Parseable[File] {
    override def parse(s: String): Try[File] = parseAndRecover(s)(new File(_))(w => s"ParseableFile: cannot interpret '$w' as a File")
  }

  implicit object ParseableFile extends ParseableFile

  /**
    * This trait splits strings of the form {x,y,z}, regardless of the format specified by the RowConfig object.
    */
  trait ParseableStringList extends Parseable[StringList] {
    override def parse(s: String): Try[StringList] = parseAndRecover(s)(split)(w => s"ParseableStringList: cannot interpret '$w' as a StringList")
  }

  implicit object ParseableStringList extends ParseableStringList

  def split(w: String): StringList = parser.parseAll(parser.list, w) match {
    case parser.Success(ws: StringList, _) => ws
    case parser.Failure(msg, _) => throw ParseableException(s"cannot split string '$w': $msg")
    case parser.Error(msg, _) => throw ParseableException(s"cannot split string '$w': $msg")
    case _ => throw ParseableException(s"cannot split string '$w'")
  }

  private val parser = new ListParser()

  private def parseAndRecover[T](w: String)(f: String => T)(msg: String => String) = Try(f(w)).recoverWith { case x: IllegalArgumentException => Failure(ParseableException(msg(w), x)) }
}

abstract class ParseableOption[T: Parseable] extends Parseable[Option[T]] {
  def parse(s: String): Try[Option[T]] = implicitly[Parseable[T]].parse(s).map(Some(_)).recoverWith { case _: ParseableException => Success(None) }
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
  * This class is a parser of lists.
  * A list is considered to be enclosed by {} and separated by commas.
  */
class ListParser() extends JavaTokenParsers {

  def list: Parser[StringList] = "{" ~> strings <~ "}" | singleton

  def strings: Parser[StringList] = repsep("""[^,}]+""".r, ",")

  def singleton: Parser[StringList] = """\w*""".r ^^ { w: String => List(w) }
}

case class ParseableException(msg: String, e: Throwable = null) extends Exception(msg, e)
