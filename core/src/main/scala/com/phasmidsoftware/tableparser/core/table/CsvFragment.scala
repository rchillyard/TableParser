/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

import java.net.URI
import scala.util.parsing.combinator.RegexParsers

/**
 * A fragment identifier of a CSV file, as defined by RFC 7111 (https://www.rfc-editor.org/rfc/rfc7111):
 * a selection of rows (e.g. "row=5-7"), of columns (e.g. "col=2") or of cells (e.g. "cell=4,1-6,2").
 * Several specifications may be separated by ";" (e.g. "row=3;6").
 *
 * As in the RFC, rows and columns are numbered from 1, and the header line(s) of a CSV file are its first rows
 * (so, for a file with one header line, row 2 is the first row of data). "*" means the last row (or column).
 * A specification which selects backwards (e.g. "row=5-4"), or which is entirely beyond the data, selects nothing;
 * a range which extends beyond the data is cut short.
 */
sealed trait CsvFragment

/**
 * A position in a CSV fragment: a row or column number (from 1), or the last one ("*").
 */
sealed trait Position:
  /**
   * @param last the number of the last row (or column).
   * @return the number of this position.
   */
  def resolve(last: Int): Int

case class At(n: Int) extends Position:
  def resolve(last: Int): Int = n

case object Last extends Position:
  def resolve(last: Int): Int = last

/**
 * A range of positions (a single position, if from and to are the same).
 *
 * @param from the first position.
 * @param to   the last position.
 */
case class Span(from: Position, to: Position):
  /**
   * Method to determine whether this Span contains the number i.
   *
   * @param i         a row (or column) number.
   * @param maybeLast the number of the last row (or column), which is needed only if "*" starts this Span.
   * @return true if i is in this Span.
   */
  def contains(i: Int, maybeLast: Option[Int]): Boolean =
    (from, maybeLast) match
      case (Last, None) => throw TableException("Span.contains: the last position is unknown")
      case _ =>
        val last = maybeLast.getOrElse(Int.MaxValue)
        from.resolve(last) <= i && i <= to.resolve(last)

  /**
   * @return true if this Span cannot be evaluated without knowing the last position.
   */
  def needsLast: Boolean = from == Last

  /**
   * @return the last number this Span could contain, if it doesn't extend to the end.
   */
  def maybeUpperBound: Option[Int] = to match
    case At(n) => Some(n)
    case Last => None

/**
 * A selection of rows (e.g. "row=5-7;10").
 *
 * @param spans the specifications.
 */
case class RowSelection(spans: Seq[Span]) extends CsvFragment:

  /**
   * Method to select, lazily, the rows of an Iterator of the rows of a CSV file which follow its header lines.
   * Only if a specification starts with "*" (the last row) must all the rows be read before any is selected;
   * otherwise, the iterator is read only as far as the last row which could be selected.
   *
   * @param xs          the rows, in order (the first follows the header lines).
   * @param headerLines the number of header lines (the first of xs is row headerLines + 1).
   * @tparam X the row type.
   * @return an Iterator of the selected rows, in order (each once, even if more than one specification selects it).
   */
  def select[X](xs: Iterator[X], headerLines: Int): Iterator[X] =
    def selected(ys: Iterator[X], maybeLast: Option[Int]): Iterator[X] =
      ys.zipWithIndex collect { case (x, k) if spans.exists(_.contains(k + 1 + headerLines, maybeLast)) => x }

    if (spans exists (_.needsLast))
      val all = xs.toList
      selected(all.iterator, Some(all.size + headerLines))
    else
      // NOTE if every specification has an upper bound, nothing beyond the highest is read.
      val bounded = if (spans.forall(_.maybeUpperBound.isDefined)) xs.take(math.max(0, spans.flatMap(_.maybeUpperBound).max - headerLines)) else xs
      selected(bounded, None)

  /**
   * Method to select the rows of a Table.
   * The header is kept: its lines count as the first rows (so, for a table with a one-line header, row 2 is its first row).
   *
   * @param table the table.
   * @tparam X the row type.
   * @return a Table of the selected rows.
   */
  def select[X](table: Table[X]): Table[X] =
    table.unit(select(table.toSeq.iterator, RowSelection.headerLines(table)).toSeq, table.maybeHeader)

object RowSelection:
  /**
   * The selection of every row.
   */
  val all: RowSelection = RowSelection(Seq(Span(At(1), Last)))

  /**
   * @param table a table.
   * @return the number of lines of its header (0 if it has none).
   */
  def headerLines(table: Table[?]): Int = table.maybeHeader.map(h => 1 + h.xss.size).getOrElse(0)

/**
 * A selection of columns (e.g. "col=2-3").
 *
 * @param spans the specifications.
 */
case class ColumnSelection(spans: Seq[Span]) extends CsvFragment:

  /**
   * Method to select the columns of a raw table (a table of a case class can't lose its fields).
   * The header is kept, with the names of the selected columns.
   *
   * @param table a RawTable.
   * @return a RawTable of the selected columns, in order (each once).
   */
  def select(table: RawTable): RawTable =
    val width = table.maybeHeader.map(_.size).getOrElse(table.headOption.map(_.ws.size).getOrElse(0))
    val columns = (1 to width).filter(i => spans.exists(_.contains(i, Some(width)))).map(_ - 1)
    CsvFragment.selectColumns(table, columns)

/**
 * A selection of cells (e.g. "cell=4,1-6,2"), each a rectangle: rows (from, to) and columns (from, to).
 *
 * @param rectangles the specifications, each a Span of rows and a Span of columns.
 */
case class CellSelection(rectangles: Seq[(Span, Span)]) extends CsvFragment:

  /**
   * Method to select the cells of a raw table: one table for each rectangle (since rectangles may be disjoint),
   * leaving out any rectangle which selects nothing. Each table keeps the header, with the names of its columns.
   *
   * @param table a RawTable.
   * @return a sequence of RawTables.
   */
  def select(table: RawTable): Seq[RawTable] =
    for
      (rows, columns) <- rectangles
      t = RowSelection(Seq(rows)).select(ColumnSelection(Seq(columns)).select(table))
      if t.nonEmpty
    yield t

object CsvFragment:

  /**
   * Method to parse a fragment identifier (the part of a URI after "#"), as RFC 7111 defines it.
   * A fragment identifier with a syntax error must be ignored (RFC 7111, section 4.1), so this gives None.
   *
   * @param w the fragment identifier, e.g. "row=5-7".
   * @return Some(CsvFragment), or None if w is not a CSV fragment identifier.
   */
  def parse(w: String): Option[CsvFragment] =
    FragmentParser.parseAll(FragmentParser.fragment, w) match
      case FragmentParser.Success(f, _) => Some(f)
      case _ => None

  /**
   * @param uri a URI.
   * @return the CsvFragment of uri, if it has one (and it is valid).
   */
  def fromURI(uri: URI): Option[CsvFragment] = Option(uri.getFragment) flatMap parse

  /**
   * @param uri a URI.
   * @return uri without its fragment.
   */
  def withoutFragment(uri: URI): URI = new URI(uri.getScheme, uri.getSchemeSpecificPart, null)

  // Select the given columns (from 0) of a raw table, and of its header.
  private[table] def selectColumns(table: RawTable, columns: Seq[Int]): RawTable =
    def pick(ws: Seq[String]): Seq[String] = columns.flatMap(ws.lift)

    val maybeHeader = table.maybeHeader.map(h => Header(pick(h.xs), h.xss map pick))
    val header = maybeHeader.getOrElse(Header(Nil, Nil))
    table.unit(table.toSeq.map(r => RawRow(pick(r.ws), header)), maybeHeader)

  // The grammar of RFC 7111, section 3.
  private object FragmentParser extends RegexParsers:
    override def skipWhitespace: Boolean = false

    lazy val fragment: Parser[CsvFragment] = rowsel | colsel | cellsel

    lazy val rowsel: Parser[CsvFragment] = "row=" ~> rep1sep(singlespec, ";") ^^ (ss => RowSelection(ss))

    lazy val colsel: Parser[CsvFragment] = "col=" ~> rep1sep(singlespec, ";") ^^ (ss => ColumnSelection(ss))

    lazy val cellsel: Parser[CsvFragment] = "cell=" ~> rep1sep(cellspec, ";") ^^ (cs => CellSelection(cs))

    lazy val singlespec: Parser[Span] = position ~ opt("-" ~> position) ^^ {
      case p ~ maybeQ => Span(p, maybeQ getOrElse p)
    }

    lazy val cellspec: Parser[(Span, Span)] = position ~ ("," ~> position) ~ opt("-" ~> position ~ ("," ~> position)) ^^ {
      case r1 ~ c1 ~ Some(r2 ~ c2) => (Span(r1, r2), Span(c1, c2))
      case r1 ~ c1 ~ None => (Span(r1, r1), Span(c1, c1))
    }

    lazy val position: Parser[Position] = "*" ^^^ Last | """\d+""".r ^^ (w => At(w.toIntOption getOrElse Int.MaxValue))
