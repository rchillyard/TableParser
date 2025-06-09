package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.AbstractTableParser.logException
import com.phasmidsoftware.tableparser.core.table.Header
import com.phasmidsoftware.tableparser.core.util.FP.{partition, sequence}
import com.phasmidsoftware.tableparser.core.util.Joinable.JoinableTInt
import com.phasmidsoftware.tableparser.core.util.{FunctionIterator, Joinable}
import scala.util.{Failure, Success, Try}

/**
 * Abstract class `InputTransformer` provides a base for transforming input data of type `Input` into output rows of type `Row`.
 *
 * The transformation is guided by a header (`Header`) that provides metadata about the input,
 * and a transformation function that maps input elements with corresponding indices into rows of type `Row`.
 *
 * This class extends from `InputPreprocessor`, inheriting utilities for mapping input data to rows while encapsulating
 * success or failure cases. Specific implementations are expected to define the behavior of inherited abstract methods.
 *
 * @param header the `Header` instance containing metadata (e.g., column names) about the input.
 * @param f      a transformation function that takes a `Header` and returns a function that maps tuples of
 *               `(Input, Int)` to `Try[Row]`. Each tuple represents an input element and its position (index),
 *               and is processed to construct a row or indicate failure.
 * @tparam Input the type of input element. The elements must satisfy the constraints imposed by the `Joinable` type class.
 * @tparam Row   the type of output row produced during the transformation process.
 * @constructor Generates an `InputTransformer` with the provided header and transformation function.
 */
abstract class InputTransformer[Input, Row](header: Header, f: Header => ((Input, Int)) => Try[Row]) extends InputPreprocessor[Input, Row] {

  /**
   * Processes an iterator of `Try[Row]` values, extracting successful rows while filtering out failed ones.
   *
   * The method takes an iterator of `Try[Row]` instances, which represent rows that may have been successfully
   * created or could contain errors. The successful rows are extracted and returned as an iterator, while any
   * failures are effectively ignored.
   *
   * @param rys an iterator of `Try[Row]` elements where each element is a `Try` representing the outcome of a row transformation.
   * @return an iterator of successfully processed rows (`Row`) extracted from the input iterator.
   */
  def processTriedRows(rys: Iterator[Try[Row]]): Iterator[Row]

  /**
   * Processes an iterator of input elements, transforming them into an iterator of output rows.
   * This method applies a two-step transformation: first, it maps the input elements into `Try[Row]` instances
   * using the `mapTsToRows` method, then it processes these `Try[Row]` instances with `processTriedRows` to
   * produce valid rows or handle errors.
   *
   * @param xs An `Iterator` of input elements of type `Input`, representing the data to be transformed.
   * @return An `Iterator` of output rows of type `Row`, resulting from the transformation process.
   */
  def processInput(xs: Iterator[Input]): Iterator[Row] =
    processTriedRows(mapTsToRows(xs))
}

/**
 * Abstract class `InputAggregator` is responsible for aggregating input data and transforming it into a processed
 * format that can be further utilized. It extends the `InputPreprocessor` trait, enabling capabilities such as
 * transforming input elements into `Try`-wrapped Rows (`mapTsToRows`) and processing those rows (`processTriedRows`).
 * The primary purpose of this class is to provide a mechanism for integrating input preprocessing and processing
 * logic in a cohesive manner.
 *
 * @tparam Input   the type of input data that will be provided for processing.
 * @tparam Row     the type of output rows that will be produced after processing the input.
 * @tparam Wrapper a higher-kinded type used to wrap the processed result, such as `Option`, `List`, or another container.
 * @param header a `Header` instance that represents the schema or structure of the input data.
 * @param f      a function taking a `Header` and returning a transformation function that maps
 *               an (Input, Int) pair to a `Try[Row]`. This function defines the logic for transforming input data
 *               into rows.
 */
abstract class InputAggregator[Input, Row, Wrapper[_]](header: Header, f: Header => ((Input, Int)) => Try[Row]) extends InputPreprocessor[Input, Row] {

  /**
   * Processes an iterator of `Try[Row]` elements, filtering out any failed attempts,
   * and wraps the resulting iterator of successful `Row` elements in a `Wrapper`.
   *
   * @param rys an iterator of `Try[Row]` where each element represents a potential result of processing a row.
   *            `Success` elements will be retained and processed, while `Failure` elements will be ignored.
   * @return a `Wrapper` containing an iterator of `Row` elements that successfully processed.
   */
  def processTriedRows(rys: Iterator[Try[Row]]): Wrapper[Iterator[Row]]

  /**
   * Processes an iterator of inputs and transforms them into a wrapped iterator of rows.
   *
   * This method combines the transformation of inputs into `Try[Row]` through `mapTsToRows`
   * and the subsequent processing of these tried rows through `processTriedRows`.
   *
   * @param xs an iterator of inputs of type `Input` to be processed.
   * @return a `Wrapper` containing an iterator of rows of type `Row`,
   *         where the transformation and processing operations have been applied to the inputs.
   */
  def processInput(xs: Iterator[Input]): Wrapper[Iterator[Row]] = processTriedRows(mapTsToRows(xs))
}

/**
 * A trait for preprocessing input data elements into rows, representing an early stage in data
 * transformation pipelines. The `InputPreprocessor` trait defines a contract for mapping input
 * data of type `Input` into rows of type `Row`, encapsulated in `Try` to handle processing outcomes.
 *
 * This abstraction is intended to facilitate robust and reusable data-parsing workflows, allowing the
 * encapsulation of individual transformations or mappings with error-handling functionality.
 * Implementations of this trait can provide specific strategies or rules for transforming input
 * into rows, effectively bridging raw input data and downstream processing.
 *
 * @tparam Input the type of input elements to be processed, e.g., raw data points or records.
 * @tparam Row   the type of rows to be produced by the mapping, e.g., structured representations
 *               of the input (such as case classes or key-value mappings).
 */
trait InputPreprocessor[Input, Row] {

  /**
   * Maps an iterator of input data elements into an iterator of `Try`-wrapped rows
   * by applying the transformation function defined in the `InputTransformer`.
   *
   * Each transformation encapsulates a potential success or failure for converting an
   * `Input` element into a `Row`, allowing downstream processing of results and errors.
   *
   * @param xs an iterator of input elements of type `Input` to be transformed.
   * @return an iterator of `Try[Row]`, where each `Try` represents the success or failure
   *         of transforming an input element into a row.
   */
  def mapTsToRows(xs: Iterator[Input]): Iterator[Try[Row]]
}

/**
 * The `IndexedInputToRowsAggregator` class is a concrete implementation of the `InputAggregator` abstract class.
 * It is used to transform an iterator of inputs into an iterator of rows, using a header and a transformation function.
 *
 * This class applies specific customization for handling indexed input data, where each input is paired with its index.
 * It includes options for multiline processing, forgiving error handling, and custom filtering based on predicates.
 *
 * This is so-called because it aggregates all of the successful row conversions into a `Try[Seq[Row]]`.
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
class IndexedInputToRowsAggregator[Input: Joinable, Row](header: Header, f: Header => ((Input, Int)) => Try[Row], multiline: Boolean, forgiving: Boolean, predicate: Try[Row] => Boolean) extends InputAggregator[Input, Row, Try](header, f) {

  /**
   * Implicit object that provides a `JoinableTInt` instance for the type `Input`.
   *
   * This object extends `JoinableTInt` for the type parameter `Input` and provides an implicit
   * `Joinable` instance for `Input`. It defines the joining behavior for tuples of type `(Input, Int)`
   * while utilizing the underlying `Joinable` instance for `Input`.
   *
   * @example Example usage:
   * {{{
   * val joinableTIntInstance = JoinableTInt
   * // use the implicit instance for operations requiring JoinableTInt[(Input, Int)]
   * }}}
   * @note This object is implicitly available in the scope where required. It relies on an implicit
   *       `Joinable[Input]` defined elsewhere in the application.
   * @see [[JoinableTInt]] for its detailed trait definition.
   */
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
   * NOTE this is not a simple iterator to iterator pipeline. We have to sequence the input to get the output.
   *
   * @param rys an iterator of `Try[Row]`, where each `Try` represents the result of processing a single row which may either succeed or fail
   * @return a `Try[Iterator[Row]]` that encapsulates either:
   *         - a success containing an iterator of all successfully processed rows, or
   *         - a failure if any of the individual rows failed during processing
   */
  def processTriedRows(rys: Iterator[Try[Row]]): Try[Iterator[Row]] = if (forgiving) {
    val (good, bad) = partition(rys)
    // CONSIDER using sequenceRev in order to save time
    bad foreach logException[Row] //AbstractTableParser.logException[Row]
    sequence(good filter predicate)
  }
  else
    sequence(rys filter predicate)
}

/**
 * A concrete implementation of `InputTransformer` that transforms an indexed sequence of inputs into rows.
 *
 * The `IndexedInputToRowsTransformer` extends the transformation pipeline provided by `InputTransformer`
 * by allowing the input of type `Input`, along with their associated indices, to be mapped to rows of type `Row`.
 * This transformation is controlled through a user-defined function (`f`) and takes a `Header` for metadata
 * and a `multiline` flag to adjust behavior based on input types.
 *
 * @param header    the header associated with the input data, providing metadata for the transformation process.
 * @param f         a transformation function parameterized on `Header`, which takes pairs `(Input, Int)` representing
 *                  input elements and their indices, and converts them into `Try[Row]` instances.
 * @param multiline a flag that controls whether the transformation should handle multiple lines in a special way.
 *                  - If `true`, the transformation uses a `FunctionIterator` to process multi-line inputs.
 *                  - If `false`, a simpler approach by zipping indices with inputs is used.
 * @tparam Input the type of input data, which must conform to the `Joinable` type class.
 * @tparam Row   the type of output rows produced by the transformation.
 */
class IndexedInputToRowsTransformer[Input: Joinable, Row](header: Header, f: Header => ((Input, Int)) => Try[Row], multiline: Boolean) extends InputTransformer[Input, Row](header, f) {

  /**
   * Implicit object representing a `JoinableTInt` instance for the type `Input`.
   *
   * This implicit ensures that the `JoinableTInt` operations (such as `join`, `zero`, and `valid`)
   * are defined for `(Input, Int)` based on the behavior of an implicitly available `Joinable[Input]` instance.
   *
   * Definitions:
   * - `Joinable`: A type class that defines how to join elements of a type, provide a zero value, and validate elements.
   * - `JoinableTInt`: A specialized `Joinable` for tuples of type `(T, Int)`, where the joining logic relies on the
   * `Joinable[T]` instance for the first element of the tuple.
   *
   * Usage:
   * - This implicit object provides `JoinableTInt` behavior for `(Input, Int)` automatically,
   * provided there is an implicit `Joinable[Input]` in scope.
   *
   * Details:
   * - The `tj` member refers to the `Joinable[Input]` instance, obtained implicitly.
   *
   */
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
  def processTriedRows(rys: Iterator[Try[Row]]): Iterator[Row] =
    (rys filter {
      case Success(_) => true
      case f@Failure(_) =>
        logException[Row](f)
        false
    }) map (_.get)
}
