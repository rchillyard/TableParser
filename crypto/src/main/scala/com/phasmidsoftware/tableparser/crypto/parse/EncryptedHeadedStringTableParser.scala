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
import scala.util.Try

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
 * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
 * @tparam X the underlying row type for which there must be evidence of a CellParser.
 */
case class EncryptedHeadedStringTableParser[X: CellParser, A: HexEncryption](encryptedRowPredicate: String => Boolean, keyFunction: String => String, override val maybeHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends HeadedStringTableParser[X](maybeHeader, forgiving, headerRowsToRead):

  private val phase2Parser: PlainTextHeadedStringTableParser[X] = PlainTextHeadedStringTableParser(None, forgiving, headerRowsToRead)

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
   *
   * @param predicate a predicate which will be applied to each X (i.e. AFTER decryption).
   * @return a TableParser of Table[X]
   */
  def setPredicate(predicate: Try[X] => Boolean): TableParser[Table[X]] = phase2Parser.setPredicate(predicate)

  /**
   * Set the value of forgiving for the plaintext TableParser.
   *
   * @param b true or false. See TableParser.
   * @return a TableParser of Table[X]
   */
  def setForgiving(b: Boolean): TableParser[Table[X]] = phase2Parser.setForgiving(b)

  /**
   * Set the value of multiline for the plaintext TableParser.
   *
   * @param b value of multiline for the plaintext TableParser. See TableParser.
   * @return a TableParser of Table[X]
   */
  def setMultiline(b: Boolean): TableParser[Table[X]] = phase2Parser.setMultiline(b)

  /**
   * Set the value of predicate for the plaintext TableParser.
   *
   * @param p predicate for the plaintext TableParser.
   * @return a TableParser of Table[X]
   */
  def setPlaintextPredicate(p: Try[X] => Boolean): TableParser[Table[X]] = phase2Parser.setPredicate(p)

  /**
   * Set the value of the row parser for the plaintext TableParser.
   *
   * @param rp the row parser for the plaintext TableParser.
   * @return a TableParser of Table[X]
   */
  def setRowParser(rp: RowParser[X, Input]): TableParser[Table[X]] = phase2Parser.setRowParser(rp)

  private def createPhase1Parser =
    def rawPredicate(ry: Try[RawRow]): Boolean = ry.map(r => encryptedRowPredicate(r.ws.head)).toOption.getOrElse(false)

    val encryptionHeader: Header = Header(Seq("key", "value"), Nil)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    given rawRowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser
    val lineParser: LineParser = LineParser.apply(using rowConfig)
    RawTableParser(rawPredicate, Some(encryptionHeader), forgiving = false, multiline = false, headerRowsToRead).setRowParser(StandardRowParser[RawRow](lineParser))

  import cats.effect.IO

  private def decryptTable(xt: RawTable): IO[Table[String]] =
    val wit: Table[IO[String]] = xt.map(row => HexEncryption.decryptRow(keyFunction)(row.ws))
    for (ws <- IO.parSequenceN(2)(wit.toSeq)) yield wit.unit(ws)
