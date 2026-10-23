/*
 * Copyright (c) 2019, 2020. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.identityMapper
import com.phasmidsoftware.tableparser.core.table.{Header, RawRow, Row}
import com.phasmidsoftware.tableparser.core.util.{Derivation, FP}
import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Trait to define the various parsers for reading case classes and their parameters from table rows.
 *
 * NOTE: In each of these cellParser methods, the CellParser has a parse method which ignores the columns.
 *
 */
trait CellParsers {

  /**
   * Method to return a CellParser[RawRow] from a Row.
   *
   * @return a CellParser[RawRow]
   */
  def rawRowCellParser: CellParser[RawRow] =
    new MultiCellParser[RawRow] {
      def parse(wo: Option[String], row: Row, columns: Header): Try[RawRow] = 
        Success(RawRow(row.ws, columns))
    }

  /**
   * Method to return a `CellParser[Seq[P]]` from a potentially unlimited set of P objects.
   * The counting of the elements starts at start (defaults to 1).
   *
   * @tparam P the underlying type of the result
   * @return a MultiCellParser[Seq[P]
   */
  def cellParserRepetition[P: CellParser : ColumnHelper](start: Int = 1): CellParser[Seq[P]] = 
    new MultiCellParser[Seq[P]] {
    // XXX used only for debugging
    override def toString: String = "MultiCellParser: cellParserRepetition"

    def parse(wo: Option[String], row: Row, columns: Header): Try[Seq[P]] = {
      def getTryP(i: Int) = summon[CellParser[P]].parse(Some(s"$i"), row, columns)

      FP.sequence(LazyList.from(start).map(getTryP).takeWhile(_.isSuccess))
    }
  }

  /**
   * Method to return a CellParser[Seq[P].
   * This is used only by unit tests.
   * CONSIDER eliminating this; only used in unit tests
   *
   * @tparam P the underlying type of the result
   * @return a MultiCellParser[Seq[P]
   */
  def cellParserSeq[P: CellParser]: CellParser[Seq[P]] = 
    new MultiCellParser[Seq[P]] {
    // XXX used only for debugging
    override def toString: String = "MultiCellParser: cellParserSeq"

    def parse(wo: Option[String], row: Row, columns: Header): Try[Seq[P]] = 
      FP.sequence(for (w <- row.ws) yield summon[CellParser[P]].parse(CellValue(w)))
  }

  /**
   * Method to return a `CellParser[Option[P]]`.
   *
   * This class is used for optional types which are non-scalar, i.e., there are no implicitly defined parsers for the Option[P].
   *
   * @tparam P the underlying type of the result
   * @return a `SingleCellParser[Option[P]]`
   */
  def cellParserOption[P: CellParser]: CellParser[Option[P]] = 
    new SingleCellParser[Option[P]] {
    // XXX used only for debugging
    override def toString: String = s"cellParserOption with $cp"

    private val cp: CellParser[P] = summon[CellParser[P]]

    def convertString(w: String): Try[Option[P]] = 
      convertToOptionP(cp.parse(CellValue(w)))

    override def parse(wo: Option[String], row: Row, columns: Header): Try[Option[P]] = 
      convertToOptionP(cp.parse(wo, row, columns))

    private def convertToOptionP(py: Try[P]) = 
      py.map(Some(_)).recoverWith {
      case _: ParseableException => 
        Success(None)
    }
  }

  /**
   * Method to return a CellParser[Option[String].
   *
   * TODO: why do we need this in addition to cellParserOption?
   *
   * CONSIDER: using BlankException as above...
   *
   * @return a SingleCellParser[Option[String]
   */
  lazy val cellParserOptionNonEmptyString: CellParser[Option[String]] = 
    new SingleCellParser[Option[String]] {
    // NOTE used only for debugging
    override def toString: String = s"cellParserOptionNonEmptyString"

    private val cp: CellParser[String] = 
      new CellParser[String]() {
      def convertString(w: String): Try[String] = 
        Success(if (w.isEmpty) null else w)

      // NOTE not used
      def parse(wo: Option[String], row: Row, columns: Header): Try[String] = 
        Failure(new UnsupportedOperationException)
    }

    def convertString(w: String): Try[Option[String]] = 
      cp.convertString(w).map(Option(_))

    // NOTE not used
    override def parse(wo: Option[String], row: Row, columns: Header): Try[Option[String]] = 
      cp.parse(wo, row, columns).map(Option(_))
  }

  /**
   * Method to return a CellParser[T] based on a function to convert a P into a T
   *
   * @param construct a function P => T.
   * @tparam P the type of the intermediate type.
   * @tparam T the underlying type of the result.
   * @return a SingleCellParser which converts a String into the intermediate type P and thence into a T
   */
  def cellParser[P: CellParser, T](construct: P => T): CellParser[T] = 
    new SingleCellParser[T] {
    // NOTE used only for debugging
    override def toString: String = "SingleCellParser (from a construct function)"

    def convertString(w: String): Try[T] = 
      summon[CellParser[P]].parse(CellValue(w)).map(construct)
  }

  /**
   * Method to return a `CellParser[T]` where `T` is a 2-ary `Product` and which is based on a function to convert a `(K,P)` into a `T`.
   * This method differs from `cellParser2` in that the parser of `P` (a `CellParser[P]`) is not found implicitly, but rather is looked up
   * dynamically depending on the value of the first parameter (of type `K`).
   *
   * @param construct a function `(K,P) => T`, usually the apply method of a case class.
   * @param parsers   a `Map[K, CellParser[P]]` which determines which particular parser of `P` will be used.
   *                  The key value looked up is the value of the first (`K`) field.
   * @tparam K the type of the conditional lookup key, which is also the type of the first field of `T`.
   *           Typically, this will be a `String`, but it could also be an Int or something more exotic.
   * @tparam P the type of the second field of the Product type `T`.
   * @param m         the Mirror of T, from which the field names come.
   * @param helper    the ColumnHelper[T].
   * @tparam T the underlying type of the result, a case class with two fields.
   * @return a `MultiCellParser` which converts Strings from a Row into the field types `K` and `P` and thence into a `T`.
   */
  inline def cellParser2Conditional[K, P, T](construct: (K, P) => T, parsers: Map[K, CellParser[P]])(using kp: CellParser[K], m: Mirror.ProductOf[T], helper: ColumnHelper[T]): CellParser[T] = {
    new ConditionalCellParser[K, P, T](constValue[m.MirroredLabel], Derivation.fieldNames[m.MirroredElemLabels], construct, parsers)
  }

  /**
   * Method to yield a ColumnHelper[T] based on an optional prefix, and some number of explicit aliases,
   *
   * @param maybePrefix an optional prefix for the column name.
   * @param aliases     a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
   * @tparam T the underlying type of the resulting ColumnHelper
   * @return a new instance of ColumnHelper[T]
   */
  def columnHelper[T](maybePrefix: Option[String], aliases: (String, String)*): ColumnHelper[T] = 
    columnHelper(identityMapper, maybePrefix, aliases*)

  /**
   * Method to yield a ColumnHelper[T] based on a column name mapper, and some number of explicit aliases,
   *
   * @param columnNameMapper a mapper of String=>String which will translate case class parameter names into column names.
   * @param aliases          a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
   * @tparam T the underlying type of the resulting ColumnHelper
   * @return a new instance of ColumnHelper[T]
   */
  def columnHelper[T](columnNameMapper: String => String, aliases: (String, String)*): ColumnHelper[T] = 
    columnHelper(columnNameMapper, None, aliases*)

  /**
   * Method to yield a ColumnHelper[T] based on a column name mapper, an optional prefix, and some number of explicit aliases,
   *
   * @param columnNameMapper a mapper of String=>String which will translate case class parameter names into column names.
   * @param maybePrefix      an optional prefix for the column name.
   * @param aliases          a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
   * @tparam T the underlying type of the resulting ColumnHelper
   * @return a new instance of ColumnHelper[T]
   */
  def columnHelper[T](columnNameMapper: String => String, maybePrefix: Option[String], aliases: (String, String)*): ColumnHelper[T] = 
    new ColumnHelper[T] {
    val _maybePrefix: Option[String] = maybePrefix
    val _aliases: Seq[(String, String)] = aliases
    val _columnNameMapper: String => String = columnNameMapper
  }

  /**
   * Method to yield a ColumnHelper[T] based on some number of explicit aliases,
   *
   * @param aliases a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
   * @tparam T the underlying type of the resulting ColumnHelper
   * @return a new instance of ColumnHelper[T]
   */
  def columnHelper[T](aliases: (String, String)*): ColumnHelper[T] = 
    columnHelper(identityMapper, aliases*)
}

/**
 * TODO resolve the naming issue here.
 */
object StdCellParsers extends CellParsers

/**
 * The CellParser made by CellParsers.cellParser2Conditional.
 *
 * @param typeName  the name of T (used in error messages).
 * @param fields    the names of the two fields of T.
 * @param construct a function (K,P) => T.
 * @param parsers   the parser of P to use for each value of K.
 * @tparam K the type of the first field of T.
 * @tparam P the type of the second field of T.
 * @tparam T the type of the result.
 */
class ConditionalCellParser[K, P, T](typeName: String, fields: List[String], construct: (K, P) => T, parsers: Map[K, CellParser[P]])(using kp: CellParser[K], helper: ColumnHelper[T]) extends MultiCellParser[T] {
  // NOTE used only for debugging
  override def toString: String = s"MultiCellParser: cellParser2Conditional for $typeName"

  def parse(wo: Option[String], row: Row, columns: Header): Try[T] =
    fields match {
      case Seq(f1, f2) =>
        for {
          k <- ProductCellParser.readCell[T, K](typeName, kp, wo, row, columns, f1)
          p <- ProductCellParser.readCell[T, P](typeName, parsers(k), wo, row, columns, f2)
        } yield construct(k, p)
      case _ =>
        Failure(ParseLogicException("incorrect number of field names (should be 2)"))
    }
}
