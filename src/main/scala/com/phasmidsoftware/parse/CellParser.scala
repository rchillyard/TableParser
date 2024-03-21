/*
 * Copyright (c) 2019. Phasmid Software
 */
package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import java.io.File
import java.net.URL
import org.joda.time.LocalDate
import scala.annotation.implicitNotFound
import scala.util.{Failure, Success, Try}

/**
 * Type-class trait CellParser[T].
 * This trait has methods to parse and to convert from String to T.
 *
 * TODO Need to define this better so that we don't have any non-implemented methods.
 *
 * @tparam T the (covariant) type of the resulting object.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of CellParser[${T}]. Typically, you should invoke a suitable method from CellParsers.")
trait CellParser[+T] {
  /**
   * Convert a String into a T.
   *
   * @param w the String to be converted.
   * @return a new instance of T wrapped in Try
   */
  def convertString(w: String): Try[T]

  /**
   * Parse a Convertible value into a T.
   *
   * @param value the Convertible value.
   * @return a new instance of T wrapped in Try.
   */
  def parse(value: Convertible): Try[T] = value match {
    case CellValue(w) => convertString(w)
    case RowValues(row, columns) => parse(None, row, columns)
    case _ => Failure(ParserException(s"CellParser: cannot convert value $value of type ${value.getClass}"))
  }

  /**
   * Method to parse an Option[String] to a T.
   *
   * @param wo      an optional String.
   * @param row     a Row of values.
   * @param columns the column names.
   *                CONSIDER do we actually need the Header parameter here?
   * @return a new instance of T.
   */
  def parse(wo: Option[String], row: Row, columns: Header): Try[T]
}

/**
 * Trait SingleCellParser, which extends CellParser[T].
 *
 * @tparam T the type of the resulting object.
 */
trait SingleCellParser[T] extends CellParser[T] {
  def parse(w: Option[String], row: Row, columns: Header): Try[T] = Failure(new UnsupportedOperationException)

  // NOTE used only for debugging
  override def toString: String = "SingleCellParser"
}

/**
 * CONSIDER renaming this to something like RowParser, or RawRowParser.
 *
 * @tparam T the type of the result.
 */
trait MultiCellParser[T] extends CellParser[T] {
  // NOTE not used
  //noinspection NotImplementedCode
  def convertString(w: String): Try[T] = Failure(new UnsupportedOperationException)

  // NOTE used only for debugging
  override def toString: String = "MultiCellParser"
}

object CellParser {

  implicit object BooleanCellParser$ extends SingleCellParser[Boolean] {
    def convertString(w: String): Try[Boolean] = implicitly[Parseable[Boolean]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BooleanCellParser$"
  }

  implicit object CharCellParser$ extends SingleCellParser[Char] {
    def convertString(w: String): Try[Char] = implicitly[Parseable[Char]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "CharCellParser$"
  }

  implicit object ByteCellParser$ extends SingleCellParser[Byte] {
    def convertString(w: String): Try[Byte] = implicitly[Parseable[Byte]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ByteCellParser$"
  }

  implicit object ShortCellParser$ extends SingleCellParser[Short] {
    def convertString(w: String): Try[Short] = implicitly[Parseable[Short]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ShortCellParser$"
  }

  implicit object IntCellParser$ extends SingleCellParser[Int] {
    def convertString(w: String): Try[Int] = implicitly[Parseable[Int]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "IntCellParser$"
  }

  implicit object LongCellParser$ extends SingleCellParser[Long] {
    def convertString(w: String): Try[Long] = implicitly[Parseable[Long]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LongCellParser$"
  }

  implicit object BigIntCellParser$ extends SingleCellParser[BigInt] {
    def convertString(w: String): Try[BigInt] = implicitly[Parseable[BigInt]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigIntCellParser$"
  }

  implicit object FloatCellParser$ extends SingleCellParser[Float] {
    def convertString(w: String): Try[Float] = implicitly[Parseable[Float]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FloatCellParser$"
  }

  implicit object DoubleCellParser$ extends SingleCellParser[Double] {
    def convertString(w: String): Try[Double] = implicitly[Parseable[Double]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "DoubleCellParser$"
  }

  implicit object BigDecimalCellParser$ extends SingleCellParser[BigDecimal] {
    def convertString(w: String): Try[BigDecimal] = implicitly[Parseable[BigDecimal]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigDecimalCellParser$"
  }

  /**
   * This parser succeeds on empty strings.
   */
  implicit object StringCellParser$ extends SingleCellParser[String] {
    def convertString(w: String): Try[String] = Success(w)

    // NOTE used only for debugging
    override def toString: String = "StringCellParser$"
  }

  implicit object DateTimeParser$ extends SingleCellParser[LocalDate] {
    def convertString(w: String): Try[LocalDate] = implicitly[Parseable[LocalDate]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "DateTimeParser$"
  }

  implicit object URLParser$ extends SingleCellParser[URL] {
    def convertString(w: String): Try[URL] = implicitly[Parseable[URL]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "URLParser$"
  }

  implicit object FileParser$ extends SingleCellParser[File] {
    def convertString(w: String): Try[File] = implicitly[Parseable[File]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FileParser$"
  }

  // TODO why do we have both OptionParser and ParseableOption?
  abstract class OptionParser[T: Parseable] extends SingleCellParser[Option[T]] {
    def convertString(w: String): Try[Option[T]] = implicitly[Parseable[T]].parse(w).map(Option(_)).recoverWith[Option[T]] {
      case _: BlankException => Success(None)
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