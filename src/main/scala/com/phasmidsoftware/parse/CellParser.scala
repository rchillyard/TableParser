package com.phasmidsoftware.parse

import java.io.File
import java.net.URL

import com.phasmidsoftware.table.{Header, Row}
import org.joda.time.LocalDate

trait CellParser[T] {
  // TODO Need to define this better so that we don't have any non-implemented methods.
  def convertString(w: String): T

  def read(value: Convertible): T = value match {
    case CellValue(w) => convertString(w)
    case RowValues(row, columns) => read(None, row, columns)
    case _ => throw FormatsException(s"CellParser: cannot convert value $value of type ${value.getClass}")
  }

  // CONSIDER do we actually need the Header parameter here?
  def read(w: Option[String], row: Row, columns: Header): T
}

trait SingleCellParser[T] extends CellParser[T] {
  def read(w: Option[String], row: Row, columns: Header): T = throw new UnsupportedOperationException

  override def toString: String = "SingleCellParser"
}

/**
  * CONSIDER renaming this to something like RowParser, or RawRowParser
  *
  * @tparam T the type of the result
  */
trait MultiCellParser[T] extends CellParser[T] {
  //noinspection NotImplementedCode
  def convertString(w: String): T = throw ParserException(s"MultiCellParser: convertString not implemented: $w")

  override def toString: String = "MultiCellParser"
}

sealed abstract class Convertible {
  def convertTo[T: CellParser]: T = cellReader.read(this)
}

case class CellValue(w: String) extends Convertible

case class RowValues(row: Row, ws: Header) extends Convertible

object RowValues {
  def apply(row: Row): RowValues = RowValues(row, row.hdr)
}

object CellParser {

  implicit object BooleanCellParser$ extends SingleCellParser[Boolean] {
    def convertString(w: String): Boolean = implicitly[Parseable[Boolean]].parse(w)

    override def toString: String = "BooleanCellParser$"
  }

  implicit object IntCellParser$ extends SingleCellParser[Int] {
    def convertString(w: String): Int = implicitly[Parseable[Int]].parse(w)

    override def toString: String = "IntCellParser$"
  }

  implicit object LongCellParser$ extends SingleCellParser[Long] {
    override def convertString(w: String): Long = implicitly[Parseable[Long]].parse(w)

    override def toString: String = "LongCellParser$"
  }

  implicit object DoubleCellParser$ extends SingleCellParser[Double] {
    override def convertString(w: String): Double = implicitly[Parseable[Double]].parse(w)

    override def toString: String = "DoubleCellParser$"
  }

  implicit object StringCellParser$ extends SingleCellParser[String] {
    override def convertString(w: String): String = w

    override def toString: String = "StringCellParser$"
  }

  implicit object DateTimeParser$ extends SingleCellParser[LocalDate] {
    override def toString: String = "DateTimeParser$"
    override def convertString(w: String): LocalDate = implicitly[Parseable[LocalDate]].parse(w)
  }

  implicit object URLParser$ extends SingleCellParser[URL] {
    override def convertString(w: String): URL = implicitly[Parseable[URL]].parse(w)

    override def toString: String = "URLParser$"
  }

  implicit object FileParser$ extends SingleCellParser[File] {
    override def convertString(w: String): File = implicitly[Parseable[File]].parse(w)

    override def toString: String = "FileParser$"
  }

  //  implicit object ListCellParser$ extends SingleCellParser[StringList] {
  //    def convertString(w: String): StringList = implicitly[Parseable[StringList]].parse(w)
  //    override def toString: String = "ListCellParser$"
  //  }


}