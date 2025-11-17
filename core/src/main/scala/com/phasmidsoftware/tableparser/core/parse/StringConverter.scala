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

  implicit object BooleanCellParser$ extends StringConverter[Boolean] {
    def convertString(w: String): Try[Boolean] =
      implicitly[Parseable[Boolean]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BooleanCellParser$"
  }

  implicit object CharCellParser$ extends StringConverter[Char] {
    def convertString(w: String): Try[Char] =
      implicitly[Parseable[Char]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "CharCellParser$"
  }

  implicit object ByteCellParser$ extends StringConverter[Byte] {
    def convertString(w: String): Try[Byte] =
      implicitly[Parseable[Byte]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ByteCellParser$"
  }

  implicit object ShortCellParser$ extends StringConverter[Short] {
    def convertString(w: String): Try[Short] =
      implicitly[Parseable[Short]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ShortCellParser$"
  }

  implicit object IntCellParser$ extends StringConverter[Int] {
    def convertString(w: String): Try[Int] =
      implicitly[Parseable[Int]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "IntCellParser$"
  }

  implicit object LongCellParser$ extends StringConverter[Long] {
    def convertString(w: String): Try[Long] =
      implicitly[Parseable[Long]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LongCellParser$"
  }

  implicit object BigIntCellParser$ extends StringConverter[BigInt] {
    def convertString(w: String): Try[BigInt] =
      implicitly[Parseable[BigInt]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigIntCellParser$"
  }

  implicit object FloatCellParser$ extends StringConverter[Float] {
    def convertString(w: String): Try[Float] =
      implicitly[Parseable[Float]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FloatCellParser$"
  }

  implicit object DoubleCellParser$ extends StringConverter[Double] {
    def convertString(w: String): Try[Double] =
      implicitly[Parseable[Double]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "DoubleCellParser$"
  }

  implicit object BigDecimalCellParser$ extends StringConverter[BigDecimal] {
    def convertString(w: String): Try[BigDecimal] =
      implicitly[Parseable[BigDecimal]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigDecimalCellParser$"
  }

  /**
   * This parser succeeds on empty strings.
   */
  implicit object StringCellParser$ extends StringConverter[String] {
    def convertString(w: String): Try[String] = Success(w)

    // NOTE used only for debugging
    override def toString: String = "StringCellParser$"
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

  // TODO why do we have both OptionParser and ParseableOption?
  abstract class OptionParser[T: Parseable] extends StringConverter[Option[T]] {
    def convertString(w: String): Try[Option[T]] =
      implicitly[Parseable[T]].parse(w).map(Option(_)).recoverWith[Option[T]] {
        case _: BlankException =>
          Success(None)
      }
  }

  implicit object BooleanOptionParser extends OptionParser[Boolean]

  implicit object IntOptionParser extends OptionParser[Int]

  implicit object LongOptionParser extends OptionParser[Long]

  implicit object DoubleOptionParser extends OptionParser[Double]

  implicit object LocalDateOptionParser extends OptionParser[LocalDate]

  implicit object URLOptionParser extends OptionParser[URL]

  implicit object FileOptionParser extends OptionParser[File]

}