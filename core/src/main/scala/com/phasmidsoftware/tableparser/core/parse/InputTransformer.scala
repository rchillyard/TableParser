package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.AbstractTableParser.logException
import com.phasmidsoftware.tableparser.core.table.Header
import com.phasmidsoftware.tableparser.core.util.FP.{partition, sequence}
import com.phasmidsoftware.tableparser.core.util.Joinable.JoinableTInt
import com.phasmidsoftware.tableparser.core.util.{FunctionIterator, Joinable}
import scala.util.{Failure, Success, Try}

/**
 * An abstract class `InputTransformer` that extends `InputPreprocessor` and defines methods for
 * transforming input data into rows, including error handling and extracting valid data rows.
 *
 * This class provides a scaffold for implementing data preprocessing pipelines by utilizing
 * input-to-row transformations (`mapTsToRows`) and processing the results to extract valid rows
 * while disregarding or handling errors (`processTriedRows`). It facilitates robust and reusable
 * approaches in data processing workflows, making it easier to define reusable transformation
 * logic for specific input data types and row structures.
 *
 * @tparam Input the type of input elements to be processed, for example, raw data records or strings.
 * @tparam Row   the type of rows to be produced after transformation, typically structured representations
 *               like case classes or tuples.
 */
abstract class InputTransformer[Input, Row]() extends InputPreprocessor[Input, Row] {

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
  def processInput(xs: Iterator[Input]): Iterator[Row]
}

/**
 * The `InputAggregator` abstract class extends the `InputPreprocessor` trait to provide additional
 * higher-level processing functionality for input data. It focuses on aggregating and processing
 * transformed rows encapsulated in `Try`, with support for filtering invalid or failed transformations
 * and wrapping the successfully processed rows in a desired container type `Wrapper[_]`.
 *
 * This class serves as an abstraction for handling the pipeline of parsing, processing, and aggregating
 * data within a flexible and reusable structure, designed to be used in applications that involve parsing
 * and pre-processing input data into structured forms.
 *
 * @tparam Input   the type of input elements to be processed, such as raw data or records.
 * @tparam Row     the type of rows that result from processing the inputs, typically structured representations.
 * @tparam Wrapper the container type used to wrap the successfully processed rows, e.g., `Option`, `Either`, or `List`.
 */
abstract class InputAggregator[Input, Row, Wrapper[_]]() extends InputPreprocessor[Input, Row] {

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
  def processInput(xs: Iterator[Input]): Wrapper[Iterator[Row]]
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
   * Processes an input iterator of type `Input` and maps its elements into rows of type `Row` while handling potential errors
   * using a specified transformation function. The function can operate in two modes:
   * - Multiline mode: input data can span multiple lines and will be joined using the provided `Joinable` type class.
   * - Single-line mode: each input element is processed independently.
   *
   * @param xs        The input iterator containing elements of type `Input`.
   * @param multiline A Boolean flag indicating whether to process input in multiline mode.
   *                    - `true`: Enables multiline processing using the provided `Joinable` type class.
   *                    - `false`: Processes each input independently.
   * @param g         A transformation function that takes a tuple `(Input, Int)`, representing an input element and its index,
   *                  and returns a `Try[Row]`. The function should encapsulate the logic for mapping input to a row, including error handling.
   * @param ev        An implicit instance of the `Joinable[(Input, Int)]` type class,
   *                  which handles joining and validating input tuples during multiline mode.
   * @return An iterator of `Try[Row]`, where each element represents either a successfully processed row or a failure.
   *         - In multiline mode, elements may span across multiple lines, joined using the logic provided by the `Joinable` type class.
   *         - In single-line mode, each input element is processed individually.
   */
  def mapTsToRows(xs: Iterator[Input])(multiline: Boolean)(g: ((Input, Int)) => Try[Row])(implicit ev: Joinable[(Input, Int)]): Iterator[Try[Row]] =
    if (multiline)
      for (ry <- new FunctionIterator[(Input, Int), Row](g)(xs.zipWithIndex)) yield ry
    else
      for (ry <- xs.zipWithIndex) yield g(ry)
}

/**
 * A trait that defines a contract for mapping an input of type `Input` to a row of type `Row` as a computation
 * that can either succeed or fail, represented by `Try[Row]`.
 *
 * InputMapper is designed to abstract the process of parsing or transforming an input into a structured
 * row representation, where the input may come from any external or internal source and the row represents
 * a meaningful structured output.
 *
 * NOTE this does not handle sequenced input. If you potentially need to handle sequenced input, use `InputPreprocessor` instead.
 *
 * @tparam Input the type of input that will be processed or parsed (e.g., raw data).
 * @tparam Row   the type of the resulting row produced by the mapping (e.g., structured data).
 */
trait InputMapper[Input, Row] extends (Input => Try[Row])

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
class IndexedInputToRowsAggregator[Input: Joinable, Row](header: Header, f: Header => ((Input, Int)) => Try[Row], multiline: Boolean, forgiving: Boolean, predicate: Try[Row] => Boolean) extends InputAggregator[Input, Row, Try]() {

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
  def processInput(xs: Iterator[Input]): Try[Iterator[Row]] =
    processTriedRows(mapTsToRows(xs)(multiline)(f(header)))

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
class IndexedInputToRowsTransformer[Input: Joinable, Row](header: Header, f: Header => ((Input, Int)) => Try[Row], multiline: Boolean) extends InputTransformer[Input, Row]() {

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
   * Processes an iterator of `Try[Row]` instances, filtering out any failures and retaining only successful results.
   * For each `Failure` encountered, the exception is logged using the `logException` method.
   * The successful `Row` objects are then extracted and returned as an iterator.
   *
   * @param rys An `Iterator` of `Try[Row]` instances to be processed, where each element represents a possible row or an error.
   * @return An `Iterator` of `Row` objects, containing only the successful results from the input.
   */
  def processTriedRows(rys: Iterator[Try[Row]]): Iterator[Row] =
    (rys filter {
      case Success(_) => true
      case f@Failure(_) =>
        logException[Row](f)
        false
    }) map (_.get)

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
    processTriedRows(mapTsToRows(xs)(multiline)(f(header)))
}
