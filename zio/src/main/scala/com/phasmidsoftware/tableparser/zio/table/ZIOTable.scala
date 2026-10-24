/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.zio.table

import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table._
import java.io.{File, FileWriter, InputStream}
import java.net.{URI, URL}
import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.util.{Failure, Try}
import zio._

/**
 * The methods here are basically copies of the same methods in Table, but resulting in IO rather than Try.
 */
object ZIOTable {

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   * NOTE: the FileWriter is flushed but not closed (see writeCSVFileRow, which closes it).
   *
   * @param file          instance of File where the output should be stored.
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   */
  def writeCSVFileIO(table: Table[Row])(file: File)(using renderer: CsvRenderer[Row], generator: CsvGenerator[Row], csvAttributes: CsvAttributes): Task[FileWriter] =
    ZIO.attemptBlocking(CsvTableFileRenderer[Row](file).render(table)).flatMap(ZIO.fromTry(_)) map { f => f.flush(); f }

  /**
   * Method to render this Table[T] as a CSV String with (maybe) header.
   *
   * @param t             the table.
   * @param csvAttributes implicit value of CsvAttributes.
   * @return a String.
   */
  def toCSV[T: CsvRenderer : CsvGenerator](t: Table[T])(using csvAttributes: CsvAttributes): Task[String] =
    ZIO.fromTry(CsvTableStringRenderer[T]().render(t) map (_.toString))

  /**
   * Primary method to parse a table from an Iterator of String.
   * This method is, in turn, invoked by all other parse methods defined below (other than parseSequence).
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterator[String], n: Int): Task[T] = ZIO.fromTry(parseTry(ws, n))

  /**
   * Method to parse a table from an Iterator of String, reading as many header rows as the TableParser expects.
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parse[T: TableParser](ws: Iterator[String]): Task[T] = parse(ws, summon[TableParser[T]].headerRowsToRead)

  /**
   * Method to parse a table from an Iterable of String.
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterable[String]): Task[T] = ZIO.suspend(parse(ws.iterator))

  /**
   * Method to parse a table from a Source.
   *
   * NOTE: the caller is responsible for closing the given Source.
   *
   * @param x the Source.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseSource[T: TableParser](x: => Source): Task[T] =
    ZIO.attemptBlocking(parseTry(x.getLines(), summon[TableParser[T]].headerRowsToRead)).flatMap(ZIO.fromTry(_))

  /**
   * Method to parse a table from a Source (with proper resource management of the source).
   *
   * @param si the Source.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](si: => Task[Source]): Task[T] =
    ZIO.acquireReleaseWith(si)(s => ZIO.attemptBlocking(s.close()).orDie)(parseSource(_))

  /**
   * Method to parse a table from a URI with an implicit encoding.
   *
   * @param u     the URI.
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](u: => URI)(using codec: Codec): Task[T] = parse(sourceFromURI(u))

  /**
   * Method to parse a table from a URI with an explicit encoding.
   *
   * @param u   the URI.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](u: => URI, enc: String): Task[T] = {
    given codec: Codec = Codec(enc)
    parse(u)
  }

  /**
   * Method to parse a table from an InputStream with an implicit encoding.
   *
   * @param i     the InputStream (call-by-name).
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseInputStream[T: TableParser](i: => InputStream)(using codec: Codec): Task[T] = parse(sourceFromInputStream(i))

  /**
   * Method to parse a table from an InputStream with an explicit encoding.
   *
   * @param si  the InputStream, wrapped in Task.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseInputStream[T: TableParser](si: Task[InputStream], enc: String): Task[T] = {
    given codec: Codec = Codec(enc)
    for (s <- si; t <- parseInputStream(s)) yield t
  }

  /**
   * Method to parse a table from a File.
   *
   * NOTE: you should use parseFile(String) if you have a pathname in String form.
   *
   * TESTME
   *
   * @param f   the File (call by name in case there is an exception thrown while constructing the file).
   * @param enc the explicit encoding.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseFile[T: TableParser](f: => File, enc: String): Task[T] = {
    given codec: Codec = Codec(enc)
    parseFile(f)
  }

  /**
   * Method to parse a table from an File.
   *
   * NOTE: you should use parseFile(String) if you have a pathname in String form.
   *
   * TESTME
   *
   * @param f     the File (call by name in case there is an exception thrown while constructing the file).
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseFile[T: TableParser](f: => File)(using codec: Codec): Task[T] = parse(sourceFromFile(f))

  /**
   * Method to parse a table from a File.
   *
   * @param pathname the file pathname.
   * @param enc      the explicit encoding.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseFile[T: TableParser](pathname: String, enc: String): Task[T] = {
    given codec: Codec = Codec(enc)
    parseFile(pathname)
  }

  /**
   * Method to parse a table from an File.
   *
   * @param pathname the file pathname.
   * @param codec    (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return a Task[T]
   */
  def parseFile[T: TableParser](pathname: String)(using codec: Codec): Task[T] = parse(sourceFromFilename(pathname))

  /**
   * Method to parse a table from an File.
   *
   * @param w     the resource name.
   * @param clazz the class for which the resource should be sought (should default to the calling class but doesn't).
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseResource[T: TableParser](w: String, clazz: Class[?] = getClass)(using codec: Codec): Task[T] = parse(sourceFromClassResource(w, clazz))

  /**
   * Method to parse a table from a URL.
   *
   * @param u the URI.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseResource[T: TableParser](u: => URL)(using codec: Codec): Task[T] = parse(u.toURI)

  /**
   * Method to parse a table from a URL with an explicit encoding.
   *
   * NOTE: the logic here is different from that of parseResource(u:=>URL)(using codec: Codec) above.
   *
   * @param u   the URL.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseResource[T: TableParser](u: => URL, enc: String): Task[T] = {
    given codec: Codec = Codec(enc)
    parseResource(u)
  }
  /**
   * Method to parse a table from a sequence of sequences of String (one per row, already split into cells).
   *
   * @param wss the rows, each a sequence of Strings.
   * @tparam T the type of the resulting table.
   * @return an Task[T]
   */
  def parseSequence[T: TableParser](wss: Iterator[Seq[String]]): Task[T] = ZIO.fromTry(Table.parseSequence(wss))

  /**
   * Method to parse a table from a File as a table of Seq[String].
   *
   * @param f                the file.
   * @param maybeFixedHeader an optional fixed header. If None (the default), we expect to find the header defined in the first line of the file.
   * @param forgiving        forcing (defaults to true). If true (the default) then an individual malformed row will not prevent subsequent rows being parsed.
   * @param codec            (implicit) the encoding.
   * @return an Task of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseFileRaw(f: File, predicate: Try[RawRow] => Boolean, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true)(using codec: Codec): Task[Table[RawRow]] = {
    given z: TableParser[Table[RawRow]] = RawTableParser.create(predicate, maybeFixedHeader, forgiving)
    parseFile[Table[RawRow]](f)
  }

  /**
   * Method to parse a table from a File as a table of Seq[String].
   *
   * @param pathname the path name.
   * @param codec    (implicit) the encoding.
   * @return an Task of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseFileRaw(pathname: String, predicate: Try[RawRow] => Boolean)(using codec: Codec): Task[Table[RawRow]] = {
    given z: TableParser[Table[RawRow]] = RawTableParser.create(predicate, None)
    parseFile[Table[RawRow]](pathname)
  }

  /**
   * Method to parse a table from a resource as a table of Seq[String].
   *
   * @param s                the resource name.
   * @param maybeFixedHeader an optional fixed header. If None (the default), we expect to find the header defined in the first line of the file.
   * @param forgiving        forcing (defaults to true). If true (the default) then an individual malformed row will not prevent subsequent rows being parsed.
   * @param clazz            the class for which the resource should be sought (defaults to the calling class).
   * @param codec            (implicit) the encoding.
   * @return an Task of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseResourceRaw(s: String, predicate: Try[RawRow] => Boolean = includeAll, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true, clazz: Class[?] = getClass)(using codec: Codec): Task[Table[RawRow]] = {
    given z: TableParser[Table[RawRow]] = RawTableParser.create(predicate, maybeFixedHeader, forgiving)
    parseResource[Table[RawRow]](s, clazz)
  }

  /**
   * Method to parse a table of raw rows from an Iterable of String.
   *
   * @param ws the Strings.
   * @return an Task of Table of RawRow
   */
  def parseRaw(ws: Iterable[String], predicate: Try[RawRow] => Boolean = includeAll, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true, multiline: Boolean = true): Task[RawTable] = {
    given z: TableParser[RawTable] = RawTableParser.create(predicate, maybeFixedHeader, forgiving, multiline)
    parse(ws.iterator)
  }

  /**
   * Method to construct one of the standard table types, given an Iterable of T and an optional header.
   *
   * @param xs          an Iterable of X.
   * @param maybeHeader an optional Header.
   * @tparam X the underlying type of xs.
   * @return a Table[X] which is either a HeadedTable or an UnheadedTable, as appropriate.
   */
  def apply[X](xs: Iterable[X], maybeHeader: Option[Header]): Table[X] = maybeHeader match {
    case Some(h) => HeadedTable(Content(xs), h)
    case None => UnheadedTable(Content(xs))
  }

  /**
   * Method to render the given Table[Row] as a CSV String with header.
   *
   * @param t             the Table[Row] to be rendered.
   * @param csvAttributes implicit value of CsvAttributes.
   * @return an Iterable[String]
   */
  def toCSVRow(t: Table[Row])(using csvAttributes: CsvAttributes): Task[String] = {
    t.maybeHeader match {
      case Some(hdr) =>
        given z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        import com.phasmidsoftware.tableparser.core.render.CsvRenderer.CsvRendererRow
        ZIO.fromTry(t.toCSV)
      case _ => ZIO.fail(TableException("toCSVRow: cannot write this Table to CSV (no header)"))
    }
  }

  /**
   * Method to render the given Table[Row] as a CSV String with header.
   *
   * @param t             the Table[Row] to be rendered.
   * @param file          the destination File for the rendering of t.
   * @param csvAttributes implicit value of CsvAttributes.
   * @return an Iterable[String]
   */
  def writeCSVFileRow(t: Table[Row], file: File)(using csvAttributes: CsvAttributes): Task[FileWriter] =
    t.maybeHeader match {
      case Some(hdr) =>
        given z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        import com.phasmidsoftware.tableparser.core.render.CsvRenderer.CsvRendererRow
        writeCSVFileIO(t)(file) flatMap (f => ZIO.attemptBlocking(f.close()).as(f))
      case _ => ZIO.fail(TableException("writeCSVFileRow: cannot write this Table to CSV (no header)"))
    }

  /**
   * Method to open source defined by an InputStream.
   *
   * @param s an InputStream.
   * @return an Task[Source].
   */
  // NOTE: opening a Source can fail (e.g. there is no such file), so it's attempted (a failure, not a defect),
  // and reading or writing a file is blocking, so it runs on the blocking pool.
  private def parseTry[T: TableParser](ws: Iterator[String], n: Int): Try[T] = summon[TableParser[T]] match {
    case parser: StringTableParser[T] => parser.parse(ws, n)
    case x => Failure(ParserException(s"parse method for Iterator[String] incompatible with tableParser: $x"))
  }

  private def sourceFromInputStream(s: => InputStream)(using codec: Codec): Task[Source] = ZIO.attemptBlocking(Source.fromInputStream(s))

  /**
   * Method to open source defined by a URI.
   *
   * @param u a URI.
   * @return an Task[Source].
   */
  private def sourceFromURI(u: => URI)(using codec: Codec): Task[Source] = ZIO.attemptBlocking(Source.fromURI(u))

  /**
   * Method to open source defined by a File.
   *
   * @param f a File.
   * @return an Task[Source].
   */
  private def sourceFromFile(f: => File)(using codec: Codec): Task[Source] = ZIO.attemptBlocking(Source.fromFile(f))

  /**
   * Method to open source defined by a File.
   *
   * @param filename a File.
   * @return an Task[Source].
   */
  private def sourceFromFilename(filename: => String)(using codec: Codec): Task[Source] = ZIO.attemptBlocking(Source.fromFile(filename))

  /**
   * Method to open a source defined by a Class and a resource name.
   *
   * @param w     the resource name.
   * @param clazz the class.
   * @return an Task[Source].
   */
  private def sourceFromClassResource(w: String, clazz: Class[?])(using codec: Codec): Task[Source] = ZIO.fromTry(Try(Source.fromURL(clazz.getResource(w))).recoverWith {
    case _: java.lang.NullPointerException => Failure(TableParserException(s"Table.sourceFromClassResource: cannot find resource '$w' relative to $clazz"))
  })
}
