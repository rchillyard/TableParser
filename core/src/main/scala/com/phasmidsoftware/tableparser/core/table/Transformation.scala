/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.identityMapper
import scala.annotation.unused

/**
 * Trait which transforms an X into a Y.
 * Basically, this is just a sub-class of Function1.
 *
 * @tparam X the input type.
 * @tparam Y the output type.
 */
trait Transformation[X, Y] extends (X => Y)

/**
 * A transformation class that applies a specific set of transformations to the rows of a `RawTable` based on matching column names.
 *
 * @param transformers A map where the keys are column names (as `String`) and the values are
 *                     `Transformation[String, String]` instances, which define how to transform data in the corresponding columns.
 *
 *                     This class implements the `Transformation[RawTable, RawTable]` trait, meaning it takes a `RawTable` as input
 *                     and produces a transformed `RawTable` as output.
 *
 *                     The transformation process is based on matching column names from the `transformers` map with the column
 *                     names in the table's header. If a match is found, the corresponding transformation is applied
 *                     to the relevant column in each row of the table.
 *
 *                     ==Example Usage==
 *                     To use this class, first construct a `RawTableTransformation` instance by providing a map of column names and their
 *                     corresponding transformations:
 * {{{
 * val transformers: Map[String, Transformation[String, String]] = Map(
 *   "column1" -> CustomTransformation1,
 *   "column2" -> CustomTransformation2
 * )
 * val transformation = RawTableTransformation(transformers)
 *
 * // Apply the transformation to a RawTable object
 * val transformedTable = transformation(rawTable)
 * }}}
 *
 *                     ==Implementation Details==
 *  - The `apply` method retrieves the table's `maybeHeader` object, which contains the column names.
 *  - For each column name in the `transformers` map, it fetches the column index using `Header.getIndex`.
 *  - The transformations are then compiled into a map associating column indices with their respective transformations.
 *  - Finally, the table's rows are transformed using the `map` method, which applies a `RawRowTransformation` to each row.
 * @see [[Transformation]]
 * @see [[RawTable]]
 * @see [[Header.getIndex]]
 */
case class RawTableTransformation(transformers: Map[String, Transformation[String, String]]) extends Transformation[RawTable, RawTable] {
  /**
   * Applies a set of transformations to a given `RawTable`.
   * The transformations are selectively applied based on the column indices determined
   * by matching keys in the `transformers` map with the column headers (if available) in the input table.
   *
   * @param t the input `RawTable` which is a representation of a table containing rows and optionally headers.
   *          Each row in the table may be transformed based on the transformations for matching columns.
   * @return a new `RawTable` where each row is transformed according to the applicable transformations.
   */
  def apply(t: RawTable): RawTable = {
    val xm: Map[Int, Transformation[String, String]] = for ((k, x) <- transformers; h <- t.maybeHeader; index <- h.getIndex(k).toOption) yield (index, x)
    t.map(RawRowTransformation(xm))
  }
}

/**
 * TESTME
 *
 * @param aggregators a Map of Transformations indexed by String.
 */
@unused
case class RawTableAggregation(aggregators: Map[String, Transformation[String, String]]) extends Transformation[RawTable, RawTable] {
  /**
   * Transforms a given `RawTable` by applying specified column-wise transformations.
   *
   * @param t the input `RawTable` to be processed. It must have a header, as transformations rely on column names.
   * @return a new `RawTable` where each row has been transformed based on the specified column transformations.
   *         The transformations are applied by mapping column names to their respective transformation logic,
   *         using the header to resolve column indices.
   */
  def apply(t: RawTable): RawTable = {
    val header = t.maybeHeader.get // there must be a header for a raw table.
    // TODO avoid use of "get" (after we have created a unit test for this)
    val xm: Map[Int, Transformation[String, String]] = for ((k, x) <- aggregators; index = header.getIndex(k).get) yield (index, x)
    t.map(RawRowTransformation(xm))
  }
}

/**
 * A transformation that projects a subset of columns from a `RawTable` based on the specified column names.
 *
 * The `RawTableProjection` class allows users to specify column names, and it produces a new `RawTable` containing only these columns.
 * Internally, it determines the indices of the specified columns and applies the projection at the row level by retaining only the corresponding values.
 *
 * @param columns A sequence of column names to include in the resulting table.
 */
case class RawTableProjection(columns: Seq[String]) extends Transformation[RawTable, RawTable] {
  /**
   * Applies a projection to a `RawTable` based on the specified column names.
   *
   * This method selects a subset of columns from the input `RawTable` and returns
   * a new `RawTable` containing only the specified columns. The projection is based
   * on the column names provided during the construction of the `RawTableProjection`.
   *
   * @param t the input `RawTable` to which the transformation will be applied.
   * @return a new `RawTable` containing only the columns specified by the `columns` parameter
   *         of the parent `RawTableProjection` class, in the same order as provided.
   */
  def apply(t: RawTable): RawTable = {
    val xs: Seq[Int] = for (k <- columns; h <- t.maybeHeader; io = h.getIndex(k).toOption; i <- io) yield i
    t.map(RawRowProjection(xs))
  }
}

/**
 * Represents a transformation that can be applied to a `RawRow`, transforming its individual string elements
 * based on a collection of column-specific transformation functions.
 *
 * This class is designed to process rows of data by applying transformations to specific columns,
 * defined by their zero-based index. Any column index not explicitly configured with a transformation
 * will default to an identity transformation, leaving the original value unchanged.
 *
 * @param transformers A map where the key is the column index (0-based) and the value is a `Transformation[String, String]`
 *                     to be applied to the corresponding column's data. If a column index is not present in this map,
 *                     an identity transformation is applied by default.
 */
case class RawRowTransformation(transformers: Map[Int, Transformation[String, String]]) extends Transformation[RawRow, RawRow] {

  def apply(r: RawRow): RawRow =
    RawRow(for ((x, i) <- r.ws.zipWithIndex; f = transformers.getOrElse(i, identityMapper)) yield f(x), r.header)
}

/**
 * A case class `RawRowProjection` that transforms a `RawRow` into another `RawRow` by selecting specific columns.
 *
 * This transformation operates based on the specified indices of the columns to be kept.
 * It selects the raw string values (`ws`) of the input `RawRow` corresponding to the provided indices
 * and produces a new `RawRow` with the filtered data, while retaining the header from the input.
 *
 * @param columns A sequence of integers representing column indices to retain in the transformation.
 * @define X The input type, which is `RawRow`.
 * @define Y The output type, which is also `RawRow`.
 */
case class RawRowProjection(columns: Seq[Int]) extends Transformation[RawRow, RawRow] {
  /**
   * Transforms a given `RawRow` into a new `RawRow` by selectively retaining columns based on the indices
   * specified in the `columns` parameter of the enclosing `RawRowProjection` class.
   *
   * For each value in the input row (`r`), if its corresponding column index exists in the `columns` sequence,
   * it is included in the resulting `RawRow`. The header of the resulting `RawRow` remains unchanged.
   *
   * @param r the input `RawRow` containing a sequence of strings (`ws`) and a header (`header`).
   * @return a new `RawRow` that contains only the filtered values as per the specified column indices
   *         and retains the original header from the input row.
   */
  def apply(r: RawRow): RawRow =
    RawRow(for ((x, i) <- r.ws.zipWithIndex; if columns contains i) yield x, r.header)
}

/**
 * A case class representing a transformation for individual cell values in a table.
 *
 * `CellTransformation` acts as a wrapper around a function `f` that maps values
 * of type `X` to values of type `Y`. It extends the `Transformation` trait,
 * which itself inherits from the `Function1` trait, allowing this transformation
 * to be used like a standard Scala function.
 *
 * @param f the function that defines the transformation logic from type `X` to type `Y`.
 * @tparam X the input type of the function `f`.
 * @tparam Y the output type of the function `f`.
 */
case class CellTransformation[X, Y](f: X => Y) extends Transformation[X, Y] {
  /**
   * Applies the transformation function `f` to the given input of type `X` and produces a result of type `Y`.
   *
   * @param x the input value of type `X` to which the transformation function is applied.
   * @return the resulting value of type `Y` after applying the transformation function `f` to the input.
   */
  def apply(x: X): Y = f(x)
}