package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.StringTableParser
import scala.io.Source
import scala.util.{Failure, Try}

/**
 * The rows of a table, parsed as they are read, without an effect library (#119): see Table.rows and its variants.
 * Only the rows which haven't yet been pulled are unread, so memory is bounded however large the input is.
 *
 * A RowIterator is AutoCloseable: it closes its source when it is exhausted, or when close is called.
 * To be sure that the source is closed even if you stop early (or an exception is thrown), use it with scala.util.Using:
 * {{{
 *   for
 *     rows <- Table.rowsFile(MovieTableParser)(Paths.get("movie_metadata.csv"))
 *     n <- Using(rows)(_.count(_.toOption.exists(_.production.country == "USA")))
 *   yield n
 * }}}
 *
 * As with parsing a Table, the parser's raw predicate and predicate select the rows.
 * If the parser is forgiving, a row which can't be parsed is logged and left out; otherwise it is a Failure.
 *
 * @tparam Row the type of the rows.
 */
trait RowIterator[Row] extends Iterator[Try[Row]] with AutoCloseable:
  /**
   * @return the header of the table (read when the RowIterator was made).
   */
  def header: Header

/**
 * Companion object for RowIterator.
 */
object RowIterator:

  /**
   * Method to make a RowIterator of the rows of a Source.
   * The header is read straight away; if it can't be, the source is closed, and the result is a Failure.
   *
   * @param parser    a StringTableParser.
   * @param selection the rows to select (RFC 7111: see CsvFragment).
   * @param source    the Source (which the RowIterator closes).
   * @return a Try of a RowIterator[parser.Row].
   */
  def apply(parser: StringTableParser[?], selection: RowSelection = RowSelection.all)(source: Source): Try[RowIterator[parser.Row]] =
    parser.rows(source.getLines(), parser.headerRowsToRead, selection).map(hr => new SourceRowIterator[parser.Row](hr._1, hr._2, source)).recoverWith:
      case x =>
        source.close()
        Failure(x)

  private class SourceRowIterator[Row](val header: Header, rows: Iterator[Try[Row]], source: Source) extends RowIterator[Row]:
    private var closed = false

    def hasNext: Boolean = !closed && (rows.hasNext || {
      close()
      false
    })

    def next(): Try[Row] =
      if hasNext then rows.next()
      else throw new NoSuchElementException("RowIterator: no more rows")

    def close(): Unit =
      if !closed then
        closed = true
        source.close()
