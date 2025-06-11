/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.parse

import cats.effect.IO
import com.phasmidsoftware.tableparser.cats.crypto.HexEncryption
import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.parser.parseHeader
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.TeeIterator
import scala.reflect.ClassTag
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
 * @tparam X the underlying row type for which there must be evidence of a CellParser and ClassTag.
 */
case class EncryptedHeadedStringTableParser[X: CellParser : ClassTag, A: HexEncryption](encryptedRowPredicate: String => Boolean, keyFunction: String => String, maybeHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends HeadedStringTableParser[X](None, false, headerRowsToRead) {

  private val phase2Parser: PlainTextHeadedStringTableParser[X] = PlainTextHeadedStringTableParser(None, forgiving, headerRowsToRead)

  /**
   * TESTME
   *
   * @param xr the sequence of Inputs, one for each row
   * @param n  the number of lines that should be used as a Header.
   *           If n == 0 == maybeHeader.empty then there is a logic error.
   * @return an IO[Table]
   */
  def parseIO(xr: Iterator[String], n: Int): IO[Table[X]] = {
    def decryptAndParse(h: Header, xt: RawTable): IO[Table[X]] = {
      phase2Parser match {
        case p: TableParser[_] => for (wt <- decryptTable(xt); xt <- IO.fromTry(p.parseRows(wt.iterator, h))) yield xt
        case _ => IO.raiseError(TableParserException("phase2Parser is not a TableParserIO"))
      }

    }


    /**
     * Parse the Input, resulting in a IO[Header]
     *
     * CONSIDER making this share the same signature as parse but for different Row type.
     *
     * @param xs a sequence of Inputs to be parsed.
     * @return a IO[Header]
     */
    def parseHeaderIO(xs: Seq[Input]): IO[Header] = IO.fromTry(parseHeader(xs))


    val sr: TeeIterator[String] = new TeeIterator(n)(xr)
    val hi: IO[Header] = IO.fromTry(parseHeader(sr.tee))
    val xti: IO[RawTable] = IO.fromTry(createPhase1Parser.parse(sr, 1)) // NOTE n=1 is a guess
    for (h <- hi; xt1 <- xti; xt2 <- decryptAndParse(h, xt1)) yield xt2
  }

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

  private def createPhase1Parser = {
    def rawPredicate(ry: Try[RawRow]): Boolean = ry.map(r => encryptedRowPredicate(r.ws.head)).toOption.getOrElse(false)

    val encryptionHeader: Header = Header(Seq("key", "value"), Nil)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    implicit val rawRowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser
    val lineParser: LineParser = LineParser.apply(rowConfig)
    RawTableParser(rawPredicate, Some(encryptionHeader), forgiving = false, multiline = false, headerRowsToRead).setRowParser(StandardRowParser[RawRow](lineParser))
  }

  import cats.effect.IO

  private def decryptTable(xt: RawTable): IO[Table[String]] = {
    val wit: Table[IO[String]] = xt.map(row => HexEncryption.decryptRow(keyFunction)(row.ws))
    for (ws <- IO.parSequenceN(2)(wit.toSeq)) yield wit.unit(ws)
  }
}
