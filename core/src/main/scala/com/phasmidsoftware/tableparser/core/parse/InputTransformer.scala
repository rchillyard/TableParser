package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.AbstractTableParser.logException
import com.phasmidsoftware.tableparser.core.table.Header
import com.phasmidsoftware.tableparser.core.util.FP.{partition, sequence}
import com.phasmidsoftware.tableparser.core.util.Joinable.JoinableTInt
import com.phasmidsoftware.tableparser.core.util.{FunctionIterator, Joinable}
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

/**
 * The `IndexedInputToTableTransformer` class is a concrete implementation of the `InputTransformer` abstract class.
 * It is used to transform an iterator of inputs into an iterator of rows, using a header and a transformation function.
 *
 * This class applies specific customization for handling indexed input data, where each input is paired with its index.
 * It includes options for multiline processing, forgiving error handling, and custom filtering based on predicates.
 *
 * @param header    the `Header` object containing metadata (e.g., column names) associated with the input data.
 * @param f         the transformation function that maps the `Header` to a function that takes a tuple of input and its index
 *                  and produces a `Try[Row]`. The wrapped `Row` may represent a successful result or an error.
 * @param multiline a flag to indicate whether the input transformation spans multiple lines (true) or is processed line-by-line (false).
 * @param forgiving a flag to determine error handling:
 *                  - If true, errors are logged without interrupting processing.
 *                  - If false, any encountered error will halt processing.
 * @param predicate a predicate function to filter `Try[Row]` values. Only rows satisfying this predicate will be considered valid.
 * @tparam Input the type of the input data, which must adhere to the `Joinable` type class.
 * @tparam Row   the type of the rows resulting from the transformation.
 */
class IndexedInputToTableTransformer[Input: Joinable, Row](header: Header, f: Header => ((Input, Int)) => Try[Row], multiline: Boolean, forgiving: Boolean, predicate: Try[Row] => Boolean) extends InputTransformer[Input, Row](header, f) {

  implicit object JoinableTInt extends JoinableTInt[Input] {
    def tj: Joinable[Input] = implicitly[Joinable[Input]]
  }

  /**
   * Maps an iterator of inputs of type `Input` to an iterator of `Try[Row]`,
   * where each input is transformed into a `Try[Row]` by applying the defined transformation function.
   *
   * @param xs an iterator of inputs of type `Input` to be transformed.
   * @return an iterator of `Try[Row]` where each element represents the result of the transformation
   *         of the corresponding input, which may either succeed (resulting in `Row`) or fail (resulting in a `Failure`).
   */
  def mapTsToRows(xs: Iterator[Input]): Iterator[Try[Row]] = if (multiline)
    for (ry <- new FunctionIterator[(Input, Int), Row](f(header))(xs.zipWithIndex)) yield ry
  else
    for (ry <- xs.zipWithIndex) yield f(header)(ry)

  /**
   * Processes an iterator of `Try` wrapped rows, transforming it into a single `Try` that contains an iterator of successfully processed rows.
   *
   * @param rys an iterator of `Try[Row]`, where each `Try` represents the result of processing a single row which may either succeed or fail
   * @return a `Try[Iterator[Row]]` that encapsulates either:
   *         - a success containing an iterator of all successfully processed rows, or
   *         - a failure if any of the individual rows failed during processing
   */
  def processTriedRows(rys: Iterator[Try[Row]]): Try[Iterator[Row]] = if (forgiving) {
    val (good, bad) = partition(rys)
    // CONSIDER using sequenceRev in order to save time
    bad foreach failureHandler //AbstractTableParser.logException[Row]
    sequence(good filter predicate)
  }
  else
    sequence(rys filter predicate)


  private def failureHandler(ry: Try[Row]): Unit =
    logException[Row](ry)

}
