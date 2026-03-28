/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.{Joinable, TeeIterator}
import scala.annotation.implicitNotFound
import scala.util.{Failure, Success, Try}

/**
 * Typeclass trait representing a row processor that defines how rows of input data can be parsed into specific types,
 * typically useful for parsing and processing tabular data with headers.
 * CONSIDER finding a common super-type between this and `TableParser`.
 *
 * @tparam Row the type that each row will be processed into.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of RowProcessor[${Row}]. Typically, you should define an instance of StringRowProcessor or, for example, MovieRowProcessor.")
trait RowProcessor[Row] {

  /**
   * The input type, typically `String`.
   */
  type Input

  /**
   * This variable determines if there is a programmed, i.e., fixed, header for the parser.
   * If its value is None, it signifies that we must look to the first line(s) of data
   * for an appropriate header.
   */
  protected val maybeHeader: Option[Header] = None

  /**
   * This indicates the number of header rows which must be read from the input.
   * If maybeHeader exists, then this property is ignored.
   */
  val headerRowsToRead: Int = 1

  /**
   * Method to determine how errors are handled.
   *
   * @return true if individual errors are logged but do not cause parsing to fail.
   */
  protected val forgiving: Boolean = false

  /**
   * Value to determine whether it is acceptable to have a quoted string span more than one line.
   *
   * @return true if quoted strings may span more than one line.
   */
  protected val multiline: Boolean = false

  /**
   * Function to determine whether or not a row should be included in the table.
   * Typically used for random sampling.
   */
  protected val predicate: Try[Row] => Boolean = includeAll

  /**
   * Method to define a row parser.
   *
   * @return a RowParser[Row, Input].
   */
  protected val rowParser: RowParser[Row, Input]
}

/**
 * Abstract class representing a row processor for handling input rows and parsing them into a specified row type.
 *
 * This class extends the `RowProcessor` type class and provides additional methods for processing iterators of input rows
 * based on headers and transformation functions. It relies on the implicit evidence of the `Joinable` type class to define
 * behavior for handling the input type.
 *
 * @tparam Row the type of rows output by this processor.
 */
abstract class AbstractRowProcessor[Row] extends RowProcessor[Row] {

  /**
   * Processes an iterator of input rows with an optional header and parses them into rows.
   *
   * NOTE this will not work as required because it will still have to collect all of the
   * individual elements inside a Try at some point.
   * That defeats the purpose of just being able to iterate.
   *
   * Depending on the presence of a fixed header and the number of rows to drop (`n`):
   * - If a fixed header is available, the processing starts after dropping the first `n` rows.
   * - If no fixed header is available but `n > 0`, the first `n` rows are extracted and used to
   * parse the header, and subsequent rows are processed accordingly.
   * - If no fixed header is available and `n <= 0`, an error is returned indicating a logic issue.
   *
   * This method heavily relies on implicit evidence (`Joinable[Input]`) to ensure the input type
   * adheres to the necessary constraints for the transformation and joining of rows.
   *
   * @param xs an iterator of inputs, where each element corresponds to a row of input data.
   * @param n  the number of initial rows to ignore (e.g., when rows are part of the header).
   *           This parameter is ignored if `maybeHeader` is defined.
   * @param ev an implicit evidence parameter ensuring the input type adheres to the `Joinable` type class,
   *           which provides methods to combine or validate input elements during processing.
   * @return a `Try` containing an iterator of parsed `Row` objects if the parsing succeeds,
   *         or a failure if any step of the process encounters an error (e.g., invalid header or processing logic).
   */
  def process(xs: Iterator[Input], n: Int)(implicit ev: Joinable[Input]): Iterator[Row] = maybeHeader match {
    case Some(h) =>
      doProcessRows(xs, rowParser.parseIndexed(h))
    case None if n > 0 =>
      val yr: TeeIterator[Input] = new TeeIterator(n)(xs)
      rowParser.parseHeader(yr.tee) match {
        case Success(h) =>
          doProcessRows(yr, rowParser.parseIndexed(h))
        case Failure(exception) =>
          throw exception
      }
    case _ =>
      throw RowProcessorException("parse: logic error: no header")
  }

  /**
   * Processes an iterator of input rows based on a provided header and a transformation function.
   *
   * This method takes an iterator of inputs, applies a transformation function to each input using
   * the provided header, and returns an iterator of processed rows. The transformation function is
   * expected to yield a `Try[Row]` where each `Row` represents a successfully transformed input.
   *
   * This method uses an implicit evidence `Joinable[Input]` to ensure the input type adheres
   * to constraints for transformation and joining operations.
   *
   * @param ts     an iterator of input elements, where each element represents a row of input data.
   * @param header an instance of `Header` that provides metadata (e.g., column names) for processing the inputs.
   * @param f      a function that generates a transformation function based on the provided `Header`.
   *               The transformation function itself takes a tuple `(Input, Int)` — representing an
   *               input element and its index — and returns a `Try[Row]` indicating the success or
   *               failure of the transformation.
   * @param ev     an implicit evidence parameter ensuring the input type `Input` conforms to the
   *               `Joinable` type class, which provides methods to join or validate input elements.
   * @return an iterator of successfully processed rows of type `Row`.
   */
  private def doProcessRows(ts: Iterator[Input], f: ((Input, Int)) => Try[Row])(implicit ev: Joinable[Input]): Iterator[Row] = {

    val inputTransformer = new IndexedInputToRowsTransformer(f, multiline)

    for (rs <- inputTransformer.processInput(ts)) yield rs
  }
}

/**
 * Abstract class to extend AbstractTableParser but with Input = String.
 * This is the normal situation where a file is a sequence of Strings, each representing one line.
 *
 * @tparam Row the row type.
 */
abstract class StringRowProcessor[Row](implicit rp: RowParser[Row, String]) extends AbstractRowProcessor[Row] {
  type Input = String

  val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

  /**
   * Processes an iterator of input rows with an optional header and parses them into rows.
   *
   * NOTE this will not work as required because it will still have to collect all of the
   * individual elements inside a Try at some point.
   * That defeats the purpose of just being able to iterate.
   *
   * Depending on the presence of a fixed header and the number of rows to drop (`n`):
   * - If a fixed header is available, the processing starts after dropping the first `n` rows.
   * - If no fixed header is available but `n > 0`, the first `n` rows are extracted and used to
   * parse the header, and subsequent rows are processed accordingly.
   * - If no fixed header is available and `n <= 0`, an error is returned indicating a logic issue.
   *
   * This method heavily relies on implicit evidence (`Joinable[Input]`) to ensure the input type
   * adheres to the necessary constraints for the transformation and joining of rows.
   *
   * @param xs an iterator of inputs, where each element corresponds to a row of input data.
   * @param n  the number of initial rows to ignore (e.g., when rows are part of the header).
   * @param ev an implicit evidence parameter ensuring the input type adheres to the `Joinable` type class,
   *           which provides methods to combine or validate input elements during processing.
   * @return a `Try` containing an iterator of parsed `Row` objects if the parsing succeeds,
   *         or a failure if any step of the process encounters an error (e.g., invalid header or processing logic).
   */
  override def process(xs: Iterator[String], n: Int)(implicit ev: Joinable[String]): Iterator[Row] = super.process(xs, n)
}

// NOTE: not currently instantiated
case class RowProcessorException(msg: String, e: Option[Throwable] = None) extends Exception(msg, e.orNull)
