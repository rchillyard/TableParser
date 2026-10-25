/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import fs2.io.file.Files
import fs2.{Pipe, Stream, text}
import com.phasmidsoftware.tableparser.cats.util.IOUsing
import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table._
import java.io.{File, FileWriter, InputStream}
import java.net.{URI, URL}
import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.util.{Failure, Try}

/**
 * The methods here are basically copies of the same methods in Table, but resulting in IO rather than Try.
 */
object IOTable:

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   *
   * @param file          instance of File where the output should be stored.
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   */
  def writeCSVFileIO(table: Table[Row])(file: File)(using renderer: CsvRenderer[Row], generator: CsvGenerator[Row], csvAttributes: CsvAttributes): IO[FileWriter] =
    blockingTry(CsvTableFileRenderer[Row](file).render(table) map { f => f.flush(); f })

  /**
   * Method to render this Table[T] as a CSV String with (maybe) header.
   *
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   * @return a String.
   */
  def toCSV[T: CsvRenderer : CsvGenerator](t: Table[T])(using csvAttributes: CsvAttributes): IO[String] =
    delayTry(CsvTableStringRenderer[T]().render(t) map (_.toString))

  /**
   * Primary method to parse a table from an Iterator of String.
   * This method is, in turn, invoked by all other parse methods defined below (other than parseSequence).
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterator[String], n: Int): IO[T] = delayTry(parseTry(ws, n))

  /**
   * Method to parse a table from an Iterator of String, reading as many header rows as the TableParser expects.
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterator[String]): IO[T] = parse(ws, summon[TableParser[T]].headerRowsToRead)

  /**
   * Method to parse a table from an Iterable of String.
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterable[String]): IO[T] = IO.defer(parse(ws.iterator))

  /**
   * Method to parse a table from a Source.
   *
   * NOTE: the caller is responsible for closing the given Source.
   *
   * @param x the Source.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseSource[T: TableParser](x: => Source): IO[T] = blockingTry(parseTry(x.getLines(), summon[TableParser[T]].headerRowsToRead))

  /**
   * Method to parse a table from a Source (with proper resource management of the source).
   *
   * @param si the Source.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](si: => IO[Source]): IO[T] = IOUsing(si)(parseSource(_))

  /**
   * Method to parse a table from a URI with an implicit encoding.
   *
   * @param u     the URI.
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](u: => URI)(using codec: Codec): IO[T] = parse(sourceFromURI(u))

  /**
   * Method to parse a table from a URI with an explicit encoding.
   *
   * @param u   the URI.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](u: => URI, enc: String): IO[T] =
    given codec: Codec = Codec(enc)
    parse(u)

  /**
   * Method to parse a table from an InputStream with an implicit encoding.
   *
   * @param i     the InputStream (call-by-name).
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseInputStream[T: TableParser](i: => InputStream)(using codec: Codec): IO[T] = parse(sourceFromInputStream(i))

  /**
   * Method to parse a table from an InputStream with an explicit encoding.
   *
   * @param si  the InputStream, wrapped in IO.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseInputStream[T: TableParser](si: IO[InputStream], enc: String): IO[T] =
    given codec: Codec = Codec(enc)
    for (s <- si; t <- parseInputStream(s)) yield t

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
   * @return an IO[T]
   */
  def parseFile[T: TableParser](f: => File, enc: String): IO[T] =
    given codec: Codec = Codec(enc)
    parseFile(f)

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
   * @return an IO[T]
   */
  def parseFile[T: TableParser](f: => File)(using codec: Codec): IO[T] = parse(sourceFromFile(f))

  /**
   * Method to parse a table from a File.
   *
   * @param pathname the file pathname.
   * @param enc      the explicit encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseFile[T: TableParser](pathname: String, enc: String): IO[T] =
    given codec: Codec = Codec(enc)
    parseFile(pathname)

  /**
   * Method to parse a table from an File.
   *
   * @param pathname the file pathname.
   * @param codec    (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return a IO[T]
   */
  def parseFile[T: TableParser](pathname: String)(using codec: Codec): IO[T] = parse(sourceFromFilename(pathname))

  /**
   * Method to parse a table from an File.
   *
   * @param w     the resource name.
   * @param clazz the class for which the resource should be sought (should default to the calling class but doesn't).
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseResource[T: TableParser](w: String, clazz: Class[?] = getClass)(using codec: Codec): IO[T] = parse(sourceFromClassResource(w, clazz))

  /**
   * Method to parse a table from a URL.
   *
   * @param u the URI.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseResource[T: TableParser](u: => URL)(using codec: Codec): IO[T] = parse(u.toURI)

  /**
   * Method to parse a table from a URL with an explicit encoding.
   *
   * NOTE: the logic here is different from that of parseResource(u:=>URL)(using codec: Codec) above.
   *
   * @param u   the URL.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseResource[T: TableParser](u: => URL, enc: String): IO[T] =
    given codec: Codec = Codec(enc)
    parseResource(u)
  /**
   * Method to parse a table from a sequence of sequences of String (one per row, already split into cells).
   *
   * @param wss the rows, each a sequence of Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseSequence[T: TableParser](wss: Iterator[Seq[String]]): IO[T] = delayTry(Table.parseSequence(wss))

  /**
   * Method to parse a table from a File as a table of Seq[String].
   *
   * @param f                the file.
   * @param maybeFixedHeader an optional fixed header. If None (the default), we expect to find the header defined in the first line of the file.
   * @param forgiving        forcing (defaults to true). If true (the default) then an individual malformed row will not prevent subsequent rows being parsed.
   * @param codec            (implicit) the encoding.
   * @return an IO of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseFileRaw(f: File, predicate: Try[RawRow] => Boolean, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true)(using codec: Codec): IO[Table[RawRow]] =
    given z: TableParser[Table[RawRow]] = RawTableParser.create(predicate, maybeFixedHeader, forgiving)
    parseFile[Table[RawRow]](f)

  /**
   * Method to parse a table from a File as a table of Seq[String].
   *
   * @param pathname the path name.
   * @param codec    (implicit) the encoding.
   * @return an IO of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseFileRaw(pathname: String, predicate: Try[RawRow] => Boolean)(using codec: Codec): IO[Table[RawRow]] =
    given z: TableParser[Table[RawRow]] = RawTableParser.create(predicate, None)
    parseFile[Table[RawRow]](pathname)

  /**
   * Method to parse a table from a resource as a table of Seq[String].
   *
   * @param s                the resource name.
   * @param maybeFixedHeader an optional fixed header. If None (the default), we expect to find the header defined in the first line of the file.
   * @param forgiving        forcing (defaults to true). If true (the default) then an individual malformed row will not prevent subsequent rows being parsed.
   * @param clazz            the class for which the resource should be sought (defaults to the calling class).
   * @param codec            (implicit) the encoding.
   * @return an IO of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseResourceRaw(s: String, predicate: Try[RawRow] => Boolean = includeAll, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true, clazz: Class[?] = getClass)(using codec: Codec): IO[Table[RawRow]] =
    given z: TableParser[Table[RawRow]] = RawTableParser.create(predicate, maybeFixedHeader, forgiving)
    parseResource[Table[RawRow]](s, clazz)

  /**
   * Method to parse a table of raw rows from an Iterable of String.
   *
   * @param ws the Strings.
   * @return an IO of Table of RawRow
   */
  def parseRaw(ws: Iterable[String], predicate: Try[RawRow] => Boolean = includeAll, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true, multiline: Boolean = true): IO[RawTable] =
    given z: TableParser[RawTable] = RawTableParser.create(predicate, maybeFixedHeader, forgiving, multiline)
    parse(ws.iterator)

  /**
   * Method to construct one of the standard table types, given an Iterable of T and an optional header.
   *
   * @param xs          an Iterable of X.
   * @param maybeHeader an optional Header.
   * @tparam X the underlying type of xs.
   * @return a Table[X] which is either a HeadedTable or an UnheadedTable, as appropriate.
   */
  def apply[X](xs: Iterable[X], maybeHeader: Option[Header]): Table[X] = maybeHeader match
    case Some(h) => HeadedTable(Content(xs), h)
    case None => UnheadedTable(Content(xs))

  /**
   * Method to render the given Table[Row] as a CSV String with header.
   *
   * @param t             the Table[Row] to be rendered.
   * @param csvAttributes implicit value of CsvAttributes.
   * @return an Iterable[String]
   */
  def toCSVRow(t: Table[Row])(using csvAttributes: CsvAttributes): IO[String] =
    t.maybeHeader match
      case Some(hdr) =>
        given z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        import com.phasmidsoftware.tableparser.core.render.CsvRenderer.CsvRendererRow
        delayTry(t.toCSV)
      case _ => IO.raiseError(TableException("toCSVRow: cannot write this Table to CSV (no header)"))

  /**
   * Method to render the given Table[Row] as a CSV String with header.
   *
   * @param t             the Table[Row] to be rendered.
   * @param file          the destination File for the rendering of t.
   * @param csvAttributes implicit value of CsvAttributes.
   * @return an Iterable[String]
   */
  def writeCSVFileRow(t: Table[Row], file: File)(using csvAttributes: CsvAttributes): IO[FileWriter] =
    t.maybeHeader match
      case Some(hdr) =>
        given z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        IOTable.writeCSVFileIO(t)(file) flatMap (f => IO.blocking(f.close()).as(f))
      case _ => IO.raiseError(TableException("writeCSVFileRow: cannot write this Table to CSV (no header)"))

  /**
   * Method to open source defined by an InputStream.
   *
   * @param s an InputStream.
   * @return an IO[Source].
   */
  // ── Streaming (fs2) ─────────────────────────────────────────────────────────

  /**
   * Method to stream the rows of a table, as they are read: the whole table is never in memory.
   * The source is opened when the stream is run, and closed when it ends (whether it completes, fails or is cancelled).
   * The header is read first (as parser reads it); then each row is parsed as it is pulled.
   * The parser's predicate selects the rows; if the parser is forgiving, a row which can't be parsed is logged and left out,
   * otherwise the stream fails with its exception.
   *
   * The parser is passed as a value (not a given) so that the type of its rows is known: for example,
   * `IOTable.stream(MovieTableParser)(source)` is a `Stream[IO, Movie]`.
   *
   * @param parser a StringTableParser (e.g. a HeadedCSVTableParser, or a RawTableParser).
   * @param source the Source (call-by-name: it is made when the stream is run).
   * @return a Stream[IO, parser.Row].
   */
  def stream(parser: StringTableParser[?])(source: => Source): Stream[IO, parser.Row] =
    streamSelected(parser, RowSelection.all)(source)

  /**
   * Method to stream the rows of a URI (see stream). If the URI has a fragment identifier which selects rows
   * (RFC 7111, e.g. "data.csv#row=5-7": see CsvFragment), only those rows are streamed, and the input is read
   * no further than the last row which could be selected.
   *
   * @param parser a StringTableParser.
   * @param uri    the URI.
   * @param codec  (implicit) the encoding.
   * @return a Stream[IO, parser.Row].
   */
  def streamURI(parser: StringTableParser[?])(uri: URI)(using codec: Codec): Stream[IO, parser.Row] =
    val selection = CsvFragment.fromURI(uri) collect { case rs: RowSelection => rs } getOrElse RowSelection.all
    streamSelected(parser, selection)(Source.fromURI(CsvFragment.withoutFragment(uri)))

  // Stream the selected rows of the source (see stream).
  private def streamSelected(parser: StringTableParser[?], selection: RowSelection)(source: => Source): Stream[IO, parser.Row] =
    for
      s <- Stream.bracket(IO.blocking(source))(s => IO.blocking(s.close()))
      hrs <- Stream.eval(IO.blocking(parser.rows(s.getLines(), parser.headerRowsToRead, selection)).flatMap(IO.fromTry))
      row <- Stream.fromBlockingIterator[IO](hrs._2, chunkSize).evalMap(IO.fromTry)
    yield row

  /**
   * Method to stream the rows of a file (see stream).
   *
   * @param parser a StringTableParser.
   * @param path   the path of the file.
   * @param codec  (implicit) the encoding.
   * @return a Stream[IO, parser.Row].
   */
  def streamFile(parser: StringTableParser[?])(path: java.nio.file.Path)(using codec: Codec): Stream[IO, parser.Row] =
    stream(parser)(Source.fromFile(path.toFile))

  /**
   * Method to stream the rows of a resource (see stream).
   *
   * @param parser a StringTableParser.
   * @param w      the name of the resource.
   * @param clazz  the class relative to which the resource is found.
   * @param codec  (implicit) the encoding.
   * @return a Stream[IO, parser.Row].
   */
  def streamResource(parser: StringTableParser[?])(w: String, clazz: Class[?] = getClass)(using codec: Codec): Stream[IO, parser.Row] =
    Stream.eval(sourceFromClassResource(w, clazz)).flatMap(s => stream(parser)(s))

  /**
   * Method to stream the rows of some lines (see stream).
   *
   * @param parser a StringTableParser.
   * @param ws     the lines.
   * @return a Stream[IO, parser.Row].
   */
  def streamLines(parser: StringTableParser[?])(ws: Iterable[String]): Stream[IO, parser.Row] =
    stream(parser)(Source.fromIterable(ws.view.flatMap(w => s"$w\n")))

  /**
   * A Pipe which renders rows as the lines of a CSV file: the header first, then one line for each row
   * (without line endings: see writeCSVFile).
   *
   * @param renderer  implicit CsvRenderer[X].
   * @param generator implicit CsvGenerator[X] (for the header).
   * @tparam X the row type.
   * @return a Pipe[IO, X, String].
   */
  def csvLines[X](using renderer: CsvRenderer[X], generator: CsvGenerator[X]): Pipe[IO, X, String] =
    val header: String = generator match
      case g: CsvProductGenerator[?] => g.toColumnNames(None, None)
      case g => g.toColumnName(None, "")
    xs => Stream.emit(header) ++ xs.map(x => renderer.render(x, Map()))

  /**
   * A Pipe which writes rows to a CSV file, as they come: the header first, then one line for each row,
   * each line ending with the CsvAttributes newline. The file is closed when the stream ends.
   *
   * @param path          the path of the file.
   * @param renderer      implicit CsvRenderer[X].
   * @param generator     implicit CsvGenerator[X] (for the header).
   * @param csvAttributes implicit CsvAttributes.
   * @tparam X the row type.
   * @return a Pipe[IO, X, Nothing].
   */
  def writeCSVFile[X](path: java.nio.file.Path)(using renderer: CsvRenderer[X], generator: CsvGenerator[X], csvAttributes: CsvAttributes): Pipe[IO, X, Nothing] =
    _.through(csvLines[X]).map(_ + csvAttributes.newline).through(text.utf8.encode).through(Files[IO].writeAll(fs2.io.file.Path.fromNioPath(path)))

  // The number of rows parsed at a time, on the blocking pool, by a stream.
  private val chunkSize = 256

  // NOTE: work is suspended (and so done each time the IO is run, not when it's made):
  // reading or writing a file is blocking, so it runs on the blocking pool.
  private def delayTry[A](t: => Try[A]): IO[A] = IO.defer(IO.fromTry(t))

  private def blockingTry[A](t: => Try[A]): IO[A] = IO.blocking(t).flatMap(IO.fromTry)

  private def parseTry[T: TableParser](ws: Iterator[String], n: Int): Try[T] = summon[TableParser[T]] match
    case parser: StringTableParser[T] => parser.parse(ws, n)
    case x => Failure(ParserException(s"parse method for Seq[String] incompatible with tableParser: $x"))

  private def sourceFromInputStream(s: => InputStream)(using codec: Codec): IO[Source] = IO.blocking(Source.fromInputStream(s))

  /**
   * Method to open source defined by a URI.
   *
   * @param u a URI.
   * @return an IO[Source].
   */
  private def sourceFromURI(u: => URI)(using codec: Codec): IO[Source] = IO.blocking(Source.fromURI(u))

  /**
   * Method to open source defined by a File.
   *
   * @param f a File.
   * @return an IO[Source].
   */
  private def sourceFromFile(f: => File)(using codec: Codec): IO[Source] = IO.blocking(Source.fromFile(f))

  /**
   * Method to open source defined by a File.
   *
   * @param filename a File.
   * @return an IO[Source].
   */
  private def sourceFromFilename(filename: => String)(using codec: Codec): IO[Source] = IO.blocking(Source.fromFile(filename))

  /**
   * Method to open a source defined by a Class and a resource name.
   *
   * @param w     the resource name.
   * @param clazz the class.
   * @return an IO[Source].
   */
  private def sourceFromClassResource(w: String, clazz: Class[?])(using codec: Codec): IO[Source] = blockingTry(Try(Source.fromURL(clazz.getResource(w))).recoverWith {
    case _: java.lang.NullPointerException => Failure(TableParserException(s"Table.sourceFromClassResource: cannot find resource '$w' relative to $clazz"))
  })
