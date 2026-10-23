/*
 * Copyright (c) 2019. Phasmid Software
 */
package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{Header, Row}
import com.phasmidsoftware.tableparser.core.util.Derivation
import java.io.File
import java.net.URL
import org.joda.time.LocalDate
import scala.annotation.{implicitNotFound, tailrec}
import scala.compiletime.{constValue, erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.util.control.NonFatal
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
trait CellParser[+T] extends StringConverter[T] {

  /**
   * Parse a Convertible value into a T.
   *
   * @param value the Convertible value.
   * @return a new instance of T wrapped in Try.
   */
  def parse(value: Convertible): Try[T] = value match {
    case CellValue(w) =>
      convertString(w)
    case RowValues(row, columns) =>
      parse(None, row, columns)
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
  def parse(w: Option[String], row: Row, columns: Header): Try[T] =
    Failure(new UnsupportedOperationException)

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
  def convertString(w: String): Try[T] =
    Failure(new UnsupportedOperationException)

  // NOTE used only for debugging
  override def toString: String = "MultiCellParser"
}

object CellParser {

  implicit object BooleanCellParser extends SingleCellParser[Boolean] {
    def convertString(w: String): Try[Boolean] =
      implicitly[Parseable[Boolean]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BooleanCellParser"
  }

  implicit object CharCellParser extends SingleCellParser[Char] {
    def convertString(w: String): Try[Char] =
      implicitly[Parseable[Char]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "CharCellParser"
  }

  implicit object ByteCellParser extends SingleCellParser[Byte] {
    def convertString(w: String): Try[Byte] =
      implicitly[Parseable[Byte]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ByteCellParser"
  }

  implicit object ShortCellParser extends SingleCellParser[Short] {
    def convertString(w: String): Try[Short] =
      implicitly[Parseable[Short]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "ShortCellParser"
  }

  implicit object IntCellParser extends SingleCellParser[Int] {
    def convertString(w: String): Try[Int] =
      implicitly[Parseable[Int]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "IntCellParser"
  }

  implicit object LongCellParser extends SingleCellParser[Long] {
    def convertString(w: String): Try[Long] =
      implicitly[Parseable[Long]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LongCellParser"
  }

  implicit object BigIntCellParser extends SingleCellParser[BigInt] {
    def convertString(w: String): Try[BigInt] =
      implicitly[Parseable[BigInt]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigIntCellParser"
  }

  implicit object FloatCellParser extends SingleCellParser[Float] {
    def convertString(w: String): Try[Float] =
      implicitly[Parseable[Float]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FloatCellParser"
  }

  implicit object DoubleCellParser extends SingleCellParser[Double] {
    def convertString(w: String): Try[Double] =
      implicitly[Parseable[Double]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "DoubleCellParser"
  }

  implicit object BigDecimalCellParser extends SingleCellParser[BigDecimal] {
    def convertString(w: String): Try[BigDecimal] =
      implicitly[Parseable[BigDecimal]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "BigDecimalCellParser"
  }

  /**
   * This parser succeeds on empty strings.
   */
  implicit object StringCellParser extends SingleCellParser[String] {
    def convertString(w: String): Try[String] = Success(w)

    // NOTE used only for debugging
    override def toString: String = "StringCellParser"
  }

  implicit object LocalDateParser extends SingleCellParser[LocalDate] {
    def convertString(w: String): Try[LocalDate] =
      implicitly[Parseable[LocalDate]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "LocalDateParser"
  }

  implicit object URLParser extends SingleCellParser[URL] {
    def convertString(w: String): Try[URL] =
      implicitly[Parseable[URL]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "URLParser"
  }

  implicit object FileParser extends SingleCellParser[File] {
    def convertString(w: String): Try[File] =
      implicitly[Parseable[File]].parse(w)

    // NOTE used only for debugging
    override def toString: String = "FileParser"
  }

  // TODO why do we have both OptionParser and ParseableOption?
  abstract class OptionParser[T: Parseable] extends SingleCellParser[Option[T]] {
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

  /**
   * Method to derive a CellParser[T] for a case class T, so that `case class T(...) derives CellParser` works.
   * This replaces the cellParser1 ... cellParser13 methods of 1.x, and has no limit on the number of fields.
   *
   * Each field is read from the column whose name is given by ColumnHelper[T] for that field's name.
   * The field names come from the Mirror (at compile time), in declaration order.
   * A field whose type is itself a case class, and which has no CellParser of its own, is derived in turn.
   *
   * NOTE: for a case class, the Mirror is its companion object. So, for a case class defined locally (inside a method),
   * don't derive its CellParser inside a companion object that you write for it: initializing that object would
   * refer to itself and never finish (a StackOverflowError). Use `derives CellParser`, or define the given alongside
   * the case class: both work for local case classes. Case classes that are not local can also derive inside
   * their companion objects.
   *
   * @param m the Mirror of T.
   * @tparam T a case class.
   * @return a CellParser[T].
   */
  inline def derived[T](using m: Mirror.ProductOf[T]): CellParser[T] =
    new ProductCellParser[T](
      constValue[m.MirroredLabel],
      Derivation.fieldNames[m.MirroredElemLabels],
      fieldParsers[m.MirroredElemTypes],
      xs => m.fromProduct(Tuple.fromArray(xs.toArray))
    )(using summonInline[ColumnHelper[T]])

  private inline def fieldParsers[Ts <: Tuple]: List[CellParser[?]] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => fieldParser[t] :: fieldParsers[ts]
  }

  // Prefer a field type's own CellParser; otherwise, if it is a case class, derive one.
  private inline def fieldParser[A]: CellParser[A] = summonFrom {
    case p: CellParser[A] => p
    case m: Mirror.ProductOf[A] => derived[A](using m)
  }
}

/**
 * The CellParser of a case class T, as derived by CellParser.derived.
 *
 * @param typeName     the name of T (used in error messages).
 * @param fieldNames   the names of the fields of T, in declaration order.
 * @param fieldParsers the CellParsers of the fields of T, in the same order.
 * @param construct    a function to construct a T from the values of its fields.
 * @param helper       the ColumnHelper[T] which maps field names to column names.
 * @tparam T the type of the result.
 */
class ProductCellParser[T](typeName: String, fieldNames: List[String], fieldParsers: List[CellParser[?]], construct: Seq[Any] => T)(using helper: ColumnHelper[T]) extends MultiCellParser[T] {

  // NOTE used only for debugging
  override def toString: String = s"MultiCellParser for $typeName"

  def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
    // The fields are read in order, stopping at the first failure.
    @tailrec
    def inner(fs: List[(String, CellParser[?])], xs: List[Any]): Try[List[Any]] = fs match {
      case Nil => Success(xs.reverse)
      case (f, p) :: tail => ProductCellParser.readCell[T, Any](typeName, p, wo, row, columns, f) match {
        case Success(x) => inner(tail, x :: xs)
        case Failure(e) => Failure(e)
      }
    }

    inner(fieldNames zip fieldParsers, Nil) map construct
  }

  /**
   * A case class with just one field can also be parsed from a single String (see issue #1).
   */
  override def convertString(w: String): Try[T] = fieldParsers match {
    case p :: Nil => p.convertString(w) map (x => construct(Seq(x)))
    case _ => Failure(new UnsupportedOperationException)
  }
}

object ProductCellParser {
  /**
   * Method to read the value of one field of a case class T from a Row.
   * The column is the one named by helper for the field (and the prefix wo, if any).
   * If there is no such column, the field's parser is given the whole row (for a field which is itself a case class).
   *
   * @param typeName   the name of T (used in error messages).
   * @param cellParser the CellParser of the field.
   * @param wo         an optional prefix (the name of the enclosing field, if any).
   * @param row        the Row.
   * @param header     the Header.
   * @param field      the name of the field.
   * @param helper     the ColumnHelper[T].
   * @tparam T the type of the case class.
   * @tparam A the type of the field.
   * @return the value of the field, wrapped in Try.
   */
  def readCell[T, A](typeName: String, cellParser: CellParser[A], wo: Option[String], row: Row, header: Header, field: String)(using helper: ColumnHelper[T]): Try[A] = {
    val columnName = helper.lookup(wo, field)
    val idx = row.getIndex(columnName)
    if (idx >= 0) for (w <- row(idx); z <- cellParser.parse(CellValue(w)).recoverWith {
      case NonFatal(e) =>
        Failure(InvalidParseException(s"Problem parsing '$w' as $typeName from $columnName at index $idx of $row", e))
    }) yield z
    else cellParser.parse(Some(columnName), row, header).recoverWith {
      case _: UnsupportedOperationException =>
        Failure(ParserException(s"$typeName: where original name is based on $wo and $field, unable to find value for column '$columnName' in '${header.xs}'"))
    }
  }
}
