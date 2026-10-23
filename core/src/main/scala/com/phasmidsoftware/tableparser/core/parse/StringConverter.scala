package com.phasmidsoftware.tableparser.core.parse

import java.io.File
import java.net.URL
import java.time.LocalDate
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

  given BooleanStringConverter: StringConverter[Boolean] with {
    def convertString(w: String): Try[Boolean] =
      summon[Parseable[Boolean]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BooleanStringConverter"
  }

  given CharStringConverter: StringConverter[Char] with {
    def convertString(w: String): Try[Char] =
      summon[Parseable[Char]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "CharStringConverter"
  }

  given ByteStringConverter: StringConverter[Byte] with {
    def convertString(w: String): Try[Byte] =
      summon[Parseable[Byte]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ByteStringConverter"
  }

  given ShortStringConverter: StringConverter[Short] with {
    def convertString(w: String): Try[Short] =
      summon[Parseable[Short]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ShortStringConverter"
  }

  given IntStringConverter: StringConverter[Int] with {
    def convertString(w: String): Try[Int] =
      summon[Parseable[Int]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "IntStringConverter"
  }

  given LongStringConverter: StringConverter[Long] with {
    def convertString(w: String): Try[Long] =
      summon[Parseable[Long]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LongStringConverter"
  }

  given BigIntStringConverter: StringConverter[BigInt] with {
    def convertString(w: String): Try[BigInt] =
      summon[Parseable[BigInt]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigIntStringConverter"
  }

  given FloatStringConverter: StringConverter[Float] with {
    def convertString(w: String): Try[Float] =
      summon[Parseable[Float]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FloatStringConverter"
  }

  given DoubleStringConverter: StringConverter[Double] with {
    def convertString(w: String): Try[Double] =
      summon[Parseable[Double]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "DoubleStringConverter"
  }

  given BigDecimalStringConverter: StringConverter[BigDecimal] with {
    def convertString(w: String): Try[BigDecimal] =
      summon[Parseable[BigDecimal]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigDecimalStringConverter"
  }

  /**
   * This parser succeeds on empty strings.
   */
  given StringStringConverter: StringConverter[String] with {
    def convertString(w: String): Try[String] = Success(w)

    // NOTE used only for debugging
    override def toString: String = "StringStringConverter"
  }

  given LocalDateParser: StringConverter[LocalDate] with {
    def convertString(w: String): Try[LocalDate] =
      summon[Parseable[LocalDate]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LocalDateParser"
  }

  given URLParser: StringConverter[URL] with {
    def convertString(w: String): Try[URL] =
      summon[Parseable[URL]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "URLParser"
  }

  given FileParser: StringConverter[File] with {
    def convertString(w: String): Try[File] =
      summon[Parseable[File]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FileParser"
  }

}