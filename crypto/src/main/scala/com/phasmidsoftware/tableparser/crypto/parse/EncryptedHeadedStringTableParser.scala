/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.crypto.parse

import cats.effect.IO
import com.phasmidsoftware.tableparser.crypto.HexEncryption
import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.parser.parseHeader
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.TeeIterator
import scala.util.{Failure, Try}

/**
 * Case class to define a StringTableParser that assumes a header to be found in the input file.
 * This class attempts to provide as much built-in functionality as possible.
 *
 * This class assumes that the names of the columns are in the first line.
 * This class implements builder with a HeadedTable object.
 * This class uses StandardRowParser of its rowParser.
 *
 * @param encryptedRowPredicate a function which takes a String and returns a Boolean.
 * @param keyFunction           a function which takes a String and returns a String (input might be ignored).
 * @param maybeHeader           None => requires that the data source has a header row.
 *                              Some(h) => specifies that the header is to be taken from h.
 *                              Defaults to None.
 *                              NOTE: that the simplest is to specify the header directly from the type X.
 * @param forgiving             if true, exceptions when parsing individual rows will be logged then ignored.
 *                              if false, any exception will terminate the parsing.
 *                              Defaults to false.
 * @param headerRowsToRead      the number of header rows expected in the input file
 *                              defaults to 1.
 * @param maybePhase2Parser     the parser of the decrypted (plaintext) rows, if it has been set up by the setters
 *                              (setPredicate, setRawPredicate, setMultiline and so on); otherwise (by default) None,
 *                              in which case it is a PlainTextHeadedStringTableParser with forgiving and headerRowsToRead.
 * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
 * @tparam X the underlying row type for which there must be evidence of a CellParser.
 */
case class EncryptedHeadedStringTableParser[X: CellParser, A: HexEncryption](encryptedRowPredicate: String => Boolean, keyFunction: String => String, override val maybeHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1, maybePhase2Parser: Option[PlainTextHeadedStringTableParser[X]] = None)
        extends HeadedStringTableParser[X](maybeHeader, forgiving, headerRowsToRead):

  private val phase2Parser: PlainTextHeadedStringTableParser[X] = maybePhase2Parser.getOrElse(PlainTextHeadedStringTableParser(None, forgiving, headerRowsToRead))

  /**
   * Method to parse an encrypted table.
   * The first n lines are the (plaintext) header.
   * Each of the other lines is a key and the encrypted row, separated by "|" (see CsvTableEncryptedFileRenderer).
   * Only the rows whose key satisfies encryptedRowPredicate are decrypted (with the key given by keyFunction),
   * and they are then parsed as plaintext rows, with the header.
   *
   * @param xr the lines of the input.
   * @param n  the number of lines which make up the header.
   * @return an IO[Table].
   */
  def parseIO(xr: Iterator[String], n: Int): IO[Table[X]] = IO.defer:
    val sr: TeeIterator[String] = new TeeIterator(n)(xr)
    for
      h <- IO.fromTry(parseHeader(sr.tee))
      // NOTE the header lines have already been taken (by the TeeIterator), so no more are read here.
      // (Up to 2.0.1, one more line was dropped: the first encrypted row.)
      xt <- IO.fromTry(createPhase1Parser.parse(sr, 0))
      wt <- decryptTable(xt)
      t <- IO.fromTry(phase2Parser.parseRows(wt.iterator, h))
    yield t

  /**
   * Method to parse an encrypted table, as Table.parse (and parseFile, parseResource, etc.) do.
   * This is parseIO, run synchronously (decryption is done in IO).
   * Up to 2.0.1, this was the parse method of HeadedStringTableParser, which doesn't decrypt.
   *
   * @param xs the lines of the input.
   * @param n  the number of lines which make up the header.
   * @return a Try[Table].
   */
  override def parse(xs: Iterator[String], n: Int = headerRowsToRead): Try[Table[X]] =
    import cats.effect.unsafe.implicits.global
    parseIO(xs, n).attempt.unsafeRunSync().toTry

  /**
   * Method to parse an encrypted table lazily, as a stream of rows (see StringTableParser.rows),
   * as IOTable.stream and ZIOTable.stream (and parseSelected) do.
   * The (plaintext) header is read now; each encrypted row whose key satisfies encryptedRowPredicate is decrypted
   * only when the rows are pulled from the iterator (in chunks, each of which is decrypted in parallel).
   * The decrypted rows are then parsed as parse does (with the plaintext parser's settings).
   * If an encrypted row can't be read or decrypted, the iterator ends with a Failure (after the rows before it).
   * Up to 2.3.0, this was StringTableParser.rows, which doesn't decrypt: it gave the encrypted rows.
   *
   * @param xs        the lines of the input.
   * @param n         the number of lines which make up the header.
   * @param selection the rows to select (RFC 7111: see CsvFragment), numbered by their lines in the encrypted input.
   * @return a Try of the header and an Iterator of the (decrypted) rows (each a Try).
   */
  override def rows(xs: Iterator[String], n: Int = headerRowsToRead, selection: RowSelection = RowSelection.all): Try[(Header, Iterator[Try[X]])] =
    val sr: TeeIterator[String] = new TeeIterator(n)(xs)
    for
      h <- parseHeader(sr.tee)
      // NOTE the header lines are given again, so that the selection numbers the rows by their lines in the input.
      (_, rys) <- createPhase1Parser.rows(sr.tee.iterator ++ sr, n, selection)
      decrypted = new DecryptedLines(rys)
      (_, xys) <- phase2Parser.setHeader(h).rows(decrypted, 0)
    yield h -> (xys ++ decrypted.maybeFailure.map(Failure(_)))

  /**
   * Set the Header for the plaintext TableParser.
   *
   * CONSIDER does this make sense to allow?
   *
   * @param header the required Header.
   * @return a TableParser of Table[X]
   */
  def setHeader(header: Header): TableParser[Table[X]] =
    throw TableParserException("it makes no sense to allow setting the header of the plaintext parser independently of the encrypted parser")

  /**
   * Set the predicate for the plaintext TableParser.
   * NOTE each of the setters gives an EncryptedHeadedStringTableParser, whose plaintext parser has the new setting.
   * (Up to 2.3.0, they gave the plaintext parser itself, which doesn't decrypt: issue #112.)
   *
   * @param predicate a predicate which will be applied to each X (i.e. AFTER decryption).
   * @return an EncryptedHeadedStringTableParser.
   */
  def setPredicate(predicate: Try[X] => Boolean): EncryptedHeadedStringTableParser[X, A] =
    withPhase2Parser(phase2Parser.setPredicate(predicate))

  /**
   * Set the value of forgiving (for this parser and its plaintext TableParser).
   *
   * @param b true or false. See TableParser.
   * @return an EncryptedHeadedStringTableParser.
   */
  def setForgiving(b: Boolean): EncryptedHeadedStringTableParser[X, A] =
    copy(forgiving = b, maybePhase2Parser = Some(phase2Parser.setForgiving(b)))

  /**
   * Set the value of multiline for the plaintext TableParser.
   *
   * @param b value of multiline for the plaintext TableParser. See TableParser.
   * @return an EncryptedHeadedStringTableParser.
   */
  def setMultiline(b: Boolean): EncryptedHeadedStringTableParser[X, A] =
    withPhase2Parser(phase2Parser.setMultiline(b))

  /**
   * Set the value of predicate for the plaintext TableParser (the same as setPredicate).
   *
   * @param p predicate for the plaintext TableParser.
   * @return an EncryptedHeadedStringTableParser.
   */
  def setPlaintextPredicate(p: Try[X] => Boolean): EncryptedHeadedStringTableParser[X, A] =
    setPredicate(p)

  /**
   * Set the value of the raw predicate for the plaintext TableParser (applied to each decrypted row before it is parsed).
   *
   * @param p the raw predicate.
   * @return an EncryptedHeadedStringTableParser.
   */
  def setRawPredicate(p: RawRow => Boolean): EncryptedHeadedStringTableParser[X, A] =
    withPhase2Parser(phase2Parser.setRawPredicate(p))

  /**
   * Set the value of the row parser for the plaintext TableParser.
   *
   * @param rp the row parser for the plaintext TableParser.
   * @return an EncryptedHeadedStringTableParser.
   */
  def setRowParser(rp: RowParser[X, Input]): EncryptedHeadedStringTableParser[X, A] =
    withPhase2Parser(phase2Parser.setRowParser(rp))

  private def withPhase2Parser(p: PlainTextHeadedStringTableParser[X]): EncryptedHeadedStringTableParser[X, A] =
    copy(maybePhase2Parser = Some(p))

  private def createPhase1Parser =
    def rawPredicate(ry: Try[RawRow]): Boolean = ry.map(r => encryptedRowPredicate(r.ws.head)).toOption.getOrElse(false)

    val encryptionHeader: Header = Header(Seq("key", "value"), Nil)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    given rawRowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser
    val lineParser: LineParser = LineParser.apply(using rowConfig)
    RawTableParser(rawPredicate, Some(encryptionHeader), forgiving = false, multiline = false, headerRowsToRead).setRowParser(StandardRowParser[RawRow](lineParser))

  import cats.effect.IO

  /**
   * The decrypted lines of the encrypted rows rys, decrypted lazily, decryptionChunk at a time (in parallel).
   * It ends at the first row which can't be read or decrypted, whose exception is then maybeFailure.
   *
   * @param rys the encrypted rows (each a key and a hex string).
   */
  private class DecryptedLines(rys: Iterator[Try[RawRow]]) extends Iterator[String]:
    import cats.effect.unsafe.implicits.global

    private var failure: Option[Throwable] = None

    private val ws: Iterator[String] =
      rys.grouped(decryptionChunk).flatMap(decryptChunk).takeWhile:
        case Left(x) =>
          failure = Some(x)
          false
        case _ => true
      .collect { case Right(w) => w }

    /**
     * The exception which ended these lines, if any (known only once they have all been read).
     */
    def maybeFailure: Option[Throwable] = failure

    def hasNext: Boolean = ws.hasNext

    def next(): String = ws.next()

    private def decryptChunk(rs: Seq[Try[RawRow]]): Seq[Either[Throwable, String]] =
      IO.parTraverseN(decryptionParallelism)(rs)(ry => IO.fromTry(ry).flatMap(r => HexEncryption.decryptRow(keyFunction)(r.ws)).attempt).unsafeRunSync()

  private val decryptionChunk = 64

  private val decryptionParallelism = 2

  private def decryptTable(xt: RawTable): IO[Table[String]] =
    val wit: Table[IO[String]] = xt.map(row => HexEncryption.decryptRow(keyFunction)(row.ws))
    for (ws <- IO.parSequenceN(decryptionParallelism)(wit.toSeq)) yield wit.unit(ws)
