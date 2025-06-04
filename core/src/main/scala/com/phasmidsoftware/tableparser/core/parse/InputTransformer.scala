package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.Header
import com.phasmidsoftware.tableparser.core.util.Joinable
import scala.util.Try

/**
 * Abstract class `InputTransformer` defines a transformation pipeline for processing inputs of type `Input` into rows of type `Row`.
 * It provides methods to:
 * - Map input data to rows wrapped in `Try` objects (`mapTsToRows`).
 * - Process `Try` wrapped rows to handle errors and extract successful transformations (`processTriedRows`).
 * - Combine these two steps to transform raw inputs into valid rows, resolving all errors (`processInput`).
 *
 * Instances of this class must define how the transformation between `Input` and `Row` occurs by supplying a function of type
 * `Header => ((Input, Int)) => Try[Row]`.
 *
 * @param header the header information, typically representing column names or metadata about the dataset.
 * @param f      a function that takes a `Header` and returns a transformation function.
 *               The transformation function maps a tuple of `Input` and its position (Int) to a `Try[Row]`.
 * @tparam Input the type of the raw input data, which must adhere to the `Joinable` type class.
 * @tparam Row   the type of the resulting row after transformation.
 */
abstract class InputTransformer[Input: Joinable, Row](header: Header, f: Header => ((Input, Int)) => Try[Row]) {

  /**
   * Maps an iterator of inputs of type `Input` to an iterator of `Try[Row]`,
   * where each input is transformed into a `Try[Row]` by applying the defined transformation function.
   *
   * @param xs an iterator of inputs of type `Input` to be transformed.
   * @return an iterator of `Try[Row]` where each element represents the result of the transformation
   *         of the corresponding input, which may either succeed (resulting in `Row`) or fail (resulting in a `Failure`).
   */
  def mapTsToRows(xs: Iterator[Input]): Iterator[Try[Row]]

  /**
   * Processes an iterator of `Try` wrapped rows, transforming it into a single `Try` that contains an iterator of successfully processed rows.
   *
   * @param rys an iterator of `Try[Row]`, where each `Try` represents the result of processing a single row which may either succeed or fail
   * @return a `Try[Iterator[Row]]` that encapsulates either:
   *         - a success containing an iterator of all successfully processed rows, or
   *         - a failure if any of the individual rows failed during processing
   */
  def processTriedRows(rys: Iterator[Try[Row]]): Try[Iterator[Row]]

  /**
   * Transforms an iterator of input elements into an iterator of successfully processed rows
   * by mapping inputs to rows and handling any processing errors.
   *
   * @param xs an `Iterator` of `Input` elements to be processed into rows.
   * @return a `Try` containing an `Iterator` of successfully processed `Row` elements if processing is successful,
   *         or a failure if an error occurs during processing.
   */
  def processInput(xs: Iterator[Input]): Try[Iterator[Row]] = processTriedRows(mapTsToRows(xs))

}
