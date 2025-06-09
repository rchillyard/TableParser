/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.{Joinable, TeeIterator, TryUsing}
import org.slf4j.{Logger, LoggerFactory}
import scala.annotation.implicitNotFound
import scala.io.Source
import scala.util.{Failure, Random, Success, Try}

/**
 * Type class to parse a set of rows as a Table.
 *
 * CONSIDER removing Table parameter: it isn't used.
 *
 * @tparam Table the Table type.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of RowProcessor[${Table}]. Typically, you should define an instance of StringRowProcessor or, for example, MovieRowProcessor.")
trait RowProcessor[Table] {

  /**
   * The row type.
   */
  type Row

  /**
   * The input type.
   */
  type Input

  /**
   * This variable determines if there is a programmed, i.e. fixed, header for the parser.
   * If its value is None, it signifies that we must look to the first line of data
   * for an appropriate header.
   */
  protected val maybeFixedHeader: Option[Header]

  /**
   * This indicates the number of header rows which must be read from the input.
   * If maybeFixedHeader exists, then this number should be zero.
   */
  val headerRowsToRead: Int

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
 * The `RowProcessor` object provides utility functions and implicit classes to facilitate
 * parsing of tabular data from various input sources (e.g., files, strings). It defines
 * methods and constructs to assist in processing tabular rows into structured data.
 */
object RowProcessor {

  /**
   * Class to allow the simplification of an expression to parse a source, given a `StringTableParser`.
   *
   * @param p a StringTableParser.
   * @tparam Table the (parametric) underlying type of `p` (`Table` will be `Table[_]`).
   */
  implicit class ImplicitParser[Table](p: StringTableParser[Table]) {

    /**
     * Method to parse a `Try[Source]` character-by-character.
     * NOTE the underlying source of `sy` will be closed after parsing has been completed.
     *
     * @param sy a `Try[Source]`.
     * @return a `Try[Table]`.
     */
    def parse(sy: Try[Source]): Try[Table] =
      sy flatMap doParse

    /**
     * Method to parse a `Source`.
     * NOTE the source `s` will be closed after parsing has been completed (no resource leaks).
     *
     * @param s a Source.
     * @return a `Try[Table]`.
     */
    private def doParse(s: Source): Try[Table] =
      TryUsing(s)(x => doParse(x.getLines()))

    /**
     * Method to parse an iterator of String.
     *
     * @param xs an `Iterator[String]`.
     * @return a `Try[Table]`.
     */
    private def doParse(xs: Iterator[String]): Try[Table] =
      p.parse(xs, 1)
  }

  val r: Random = new Random()

  val logger: Logger = LoggerFactory.getLogger(TableParser.getClass)

  /**
   * Method to return a random sampling function.
   *
   * CONSIDER using FP.sampler
   *
   * @param n this is the sample factor: approximately one in every n successful results will form part of the result.
   * @tparam X the underlying type of the sampler.
   * @return a Try[X] => Boolean function which is always yields false if its input is a failure, otherwise,
   *         it chooses every nth value (approximately).
   */
  def sampler[X](n: Int): Try[X] => Boolean = {
    case Success(_) =>
      r.nextInt(n) == 0
    case _ =>
      false
  }

  /**
   * a function which always evaluates as true, regardless of the successfulness of the input.
   */
  val includeAll: Try[Any] => Boolean = _ => true
}

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
   * @param ev an implicit evidence parameter ensuring the input type adheres to the `Joinable` type class,
   *           which provides methods to combine or validate input elements during processing.
   * @return a `Try` containing an iterator of parsed `Row` objects if the parsing succeeds,
   *         or a failure if any step of the process encounters an error (e.g., invalid header or processing logic).
   */
  def process(xs: Iterator[Input], n: Int)(implicit ev: Joinable[Input]): Iterator[Row] = maybeFixedHeader match {
    case Some(h) =>
      doProcessRows(xs drop n, h, rowParser.parse) // CONSIDER reverting to check that n = 0
    case None if n > 0 =>
      val yr: TeeIterator[Input] = new TeeIterator(n)(xs)
      rowParser.parseHeader(yr.tee) match {
        case Success(h) =>
          doProcessRows(yr, h, rowParser.parse)
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
  def doProcessRows(ts: Iterator[Input], header: Header, f: Header => ((Input, Int)) => Try[Row])(implicit ev: Joinable[Input]): Iterator[Row] = {

    val inputTransformer = new IndexedInputToRowsTransformer(header, f, multiline)

    for (rs <- inputTransformer.processInput(ts)) yield rs
  }
}

/**
 * Abstract class to extend AbstractTableParser but with Input = String.
 * This is the normal situation where a file is a sequence of Strings, each representing one line.
 *
 * @tparam Row the row type.
 */
abstract class StringRowProcessor[Row] extends AbstractRowProcessor[Row] {
  type Input = String

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
