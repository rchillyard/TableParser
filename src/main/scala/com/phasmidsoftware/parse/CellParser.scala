/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import java.io.File
import java.net.URL

import com.phasmidsoftware.table.{Header, Row}
import org.joda.time.LocalDate

import scala.annotation.implicitNotFound
import scala.util.Try

/**
	* Type class trait CellParser[T].
	* This trait has methods to parse and to convert from String to T.
	*
	* TODO Need to define this better so that we don't have any non-implemented methods.
	*
	* @tparam T the type of the resulting object.
	*/
@implicitNotFound(msg = "Cannot find an implicit instance of CellParser[${T}]. Typically, you should invoke a suitable method from CellParsers.")
trait CellParser[T] {
	/**
		* Convert a String into a T.
		* @param w the String to be converted.
		* @return a new instance of T.
		*/
	def convertString(w: String): T

	/**
		* Parse a Convertible value into a T.
		* @param value the Convertible value.
		* @return a new instance of T.
		*/
	def parse(value: Convertible): T = value match {
		case CellValue(w) => convertString(w)
		case RowValues(row, columns) => parse(None, row, columns)
		case _ => throw ParsersException(s"CellParser: cannot convert value $value of type ${value.getClass}")
	}

	/**
		* Method to parse an Option[String] to a T.
		* @param wo an optional String.
		* @param row a Row of values.
		* @param columns the column names.
		*                CONSIDER do we actually need the Header parameter here?
		* @return a new instance of T.
		*/
	def parse(wo: Option[String], row: Row, columns: Header): T
}

/**
	* Trait SingleCellParser, which extends CellParser[T].
	*
	* @tparam T the type of the resulting object.
	*/
trait SingleCellParser[T] extends CellParser[T] {
	def parse(w: Option[String], row: Row, columns: Header): T = throw new UnsupportedOperationException

	override def toString: String = "SingleCellParser"
}

/**
	* CONSIDER renaming this to something like RowParser, or RawRowParser.
	*
	* @tparam T the type of the result.
	*/
trait MultiCellParser[T] extends CellParser[T] {
	//noinspection NotImplementedCode
	def convertString(w: String): T = throw new UnsupportedOperationException

	override def toString: String = "MultiCellParser"
}

/**
	* Abstract class Convertible.
	*
	* CONSIDER making this a trait since it takes no value paramters.
	*/
sealed abstract class Convertible {
	def convertTo[T: CellParser]: Try[T] = Try(cellReader.parse(this))
}

/**
	* A concrete Convertible corresponding to a cell value.
	*
	* @param w the String contained by a cell.
	*/
case class CellValue(w: String) extends Convertible

/**
	* A concrete Convertible corresponding to a row.
	* @param row the Row containing several values.
	* @param header the Header.
	*/
case class RowValues(row: Row, header: Header) extends Convertible

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

	implicit object BigIntCellParser$ extends SingleCellParser[BigInt] {
		override def convertString(w: String): BigInt = implicitly[Parseable[BigInt]].parse(w)

		override def toString: String = "BigIntCellParser$"
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

	abstract class OptionParser[T: Parseable] extends SingleCellParser[Option[T]] {
		override def convertString(w: String): Option[T] = Try(implicitly[Parseable[T]].parse(w)).toOption
	}

	implicit object BooleanOptionParser extends OptionParser[Boolean]

	implicit object IntOptionParser extends OptionParser[Int]

	implicit object LongOptionParser extends OptionParser[Long]

	implicit object DoubleOptionParser extends OptionParser[Double]

	implicit object LocalDateOptionParser extends OptionParser[LocalDate]

	implicit object URLOptionParser extends OptionParser[URL]

	implicit object FileOptionParser extends OptionParser[File]

}