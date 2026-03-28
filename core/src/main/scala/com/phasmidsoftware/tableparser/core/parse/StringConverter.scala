package com.phasmidsoftware.tableparser.core.parse

import java.io.File
import java.net.URL
import org.joda.time.LocalDate
import scala.util.{Success, Try}

trait StringConverter[+T] {

  /**
   * Convert a String into a T.
   *
   * @param w the String to be converted.
   * @return a new instance of T wrapped in Try
   */
  def convertString(w: String): Try[T]

}

object StringConverter {

  implicit object BooleanStringConverter$ extends StringConverter[Boolean] {
    def convertString(w: String): Try[Boolean] =
      implicitly[Parseable[Boolean]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BooleanStringConverter$"
  }

  implicit object CharStringConverter$ extends StringConverter[Char] {
    def convertString(w: String): Try[Char] =
      implicitly[Parseable[Char]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "CharStringConverter$"
  }

  implicit object ByteStringConverter$ extends StringConverter[Byte] {
    def convertString(w: String): Try[Byte] =
      implicitly[Parseable[Byte]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ByteStringConverter$"
  }

  implicit object ShortStringConverter$ extends StringConverter[Short] {
    def convertString(w: String): Try[Short] =
      implicitly[Parseable[Short]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ShortStringConverter$"
  }

  implicit object IntStringConverter$ extends StringConverter[Int] {
    def convertString(w: String): Try[Int] =
      implicitly[Parseable[Int]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "IntStringConverter$"
  }

  implicit object LongStringConverter$ extends StringConverter[Long] {
    def convertString(w: String): Try[Long] =
      implicitly[Parseable[Long]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LongStringConverter$"
  }

  implicit object BigIntStringConverter$ extends StringConverter[BigInt] {
    def convertString(w: String): Try[BigInt] =
      implicitly[Parseable[BigInt]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigIntStringConverter$"
  }

  implicit object FloatStringConverter$ extends StringConverter[Float] {
    def convertString(w: String): Try[Float] =
      implicitly[Parseable[Float]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FloatStringConverter$"
  }

  implicit object DoubleStringConverter$ extends StringConverter[Double] {
    def convertString(w: String): Try[Double] =
      implicitly[Parseable[Double]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "DoubleStringConverter$"
  }

  implicit object BigDecimalStringConverter$ extends StringConverter[BigDecimal] {
    def convertString(w: String): Try[BigDecimal] =
      implicitly[Parseable[BigDecimal]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigDecimalStringConverter$"
  }

  /**
   * This parser succeeds on empty strings.
   */
  implicit object StringStringConverter$ extends StringConverter[String] {
    def convertString(w: String): Try[String] = Success(w)

    // NOTE used only for debugging
    override def toString: String = "StringStringConverter$"
  }

  implicit object LocalDateParser$ extends StringConverter[LocalDate] {
    def convertString(w: String): Try[LocalDate] =
      implicitly[Parseable[LocalDate]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LocalDateParser$"
  }

  implicit object URLParser$ extends StringConverter[URL] {
    def convertString(w: String): Try[URL] =
      implicitly[Parseable[URL]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "URLParser$"
  }

  implicit object FileParser$ extends StringConverter[File] {
    def convertString(w: String): Try[File] =
      implicitly[Parseable[File]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FileParser$"
  }

}