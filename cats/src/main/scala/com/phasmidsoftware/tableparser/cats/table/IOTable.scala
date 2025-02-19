/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.IOUsing
import java.io.{File, FileWriter, InputStream}
import java.net.{URI, URL}
import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.util.{Failure, Try}

/**
 * The methods here are basically copies of the same methods in Table, but resulting in IO rather than Try.
 */
object IOTable {

  /**
   * Method to render this Table[T] as a CSV String with (maybe) header.
   *
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   * @return a String.
   */
  def toCSV[T: CsvRenderer : CsvGenerator](t: Table[T])(implicit renderer: CsvRenderer[Row], generator: CsvGenerator[Row], csvAttributes: CsvAttributes): IO[String] =
    IO.fromTry(CsvTableStringRenderer[T]().render(t) map (_.toString))

  /**
   * Primary method to parse a table from an Iterator of String.
   * This method is, in turn, invoked by all other parse methods defined below (other than parseSequence).
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterator[String], n: Int): IO[T] = implicitly[TableParser[T]] match {
    case parser: StringTableParser[T] => IO.fromTry(parser.parse(ws, n))
    case x => IO.raiseError(ParserException(s"parse method for Seq[String] incompatible with tableParser: $x"))
  }

  /**
   * Method to parse a table from an Iterable of String.
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterable[String]): IO[T] = parse(ws.iterator, 0)

  /**
   * Method to parse a table from a Source.
   *
   * NOTE: the caller is responsible for closing the given Source.
   *
   * @param x the Source.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseSource[T: TableParser](x: => Source): IO[T] = for (y <- parse(x.getLines(), 0)) yield y

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
  def parse[T: TableParser](u: => URI)(implicit codec: Codec): IO[T] = parse(sourceFromURI(u))

  /**
   * Method to parse a table from a URI with an explicit encoding.
   *
   * @param u   the URI.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](u: => URI, enc: String): IO[T] = {
    implicit val codec: Codec = Codec(enc)
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
  def parseInputStream[T: TableParser](i: => InputStream)(implicit codec: Codec): IO[T] = parse(sourceFromInputStream(i))

  /**
   * Method to parse a table from an InputStream with an explicit encoding.
   *
   * @param si  the InputStream, wrapped in IO.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseInputStream[T: TableParser](si: IO[InputStream], enc: String): IO[T] = {
    implicit val codec: Codec = Codec(enc)
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
   * @return an IO[T]
   */
  def parseFile[T: TableParser](f: => File, enc: String): IO[T] = {
    implicit val codec: Codec = Codec(enc)
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
   * @return an IO[T]
   */
  def parseFile[T: TableParser](f: => File)(implicit codec: Codec): IO[T] = parse(sourceFromFile(f))

  /**
   * Method to parse a table from a File.
   *
   * @param pathname the file pathname.
   * @param enc      the explicit encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseFile[T: TableParser](pathname: String, enc: String): IO[T] = {
    implicit val codec: Codec = Codec(enc)
    parseFile(pathname)
  }

  /**
   * Method to parse a table from an File.
   *
   * @param pathname the file pathname.
   * @param codec    (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return a IO[T]
   */
  def parseFile[T: TableParser](pathname: String)(implicit codec: Codec): IO[T] = parse(sourceFromFilename(pathname))

  /**
   * Method to parse a table from an File.
   *
   * @param w     the resource name.
   * @param clazz the class for which the resource should be sought (should default to the calling class but doesn't).
   * @param codec (implicit) the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseResource[T: TableParser](w: String, clazz: Class[_] = getClass)(implicit codec: Codec): IO[T] = parse(sourceFromClassResource(w, clazz))

  /**
   * Method to parse a table from a URL.
   *
   * @param u the URI.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseResource[T: TableParser](u: => URL)(implicit codec: Codec): IO[T] = parse(u.toURI)

  /**
   * Method to parse a table from a URL with an explicit encoding.
   *
   * NOTE: the logic here is different from that of parseResource(u:=>URL)(implicit codec: Codec) above.
   *
   * @param u   the URL.
   * @param enc the encoding.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseResource[T: TableParser](u: => URL, enc: String): IO[T] = {
    implicit val codec: Codec = Codec(enc)
    parseResource(u)
  }
//
//  /**
//   * Method to parse a table from a Seq of Seq of String.
//   *
//   * @param wss the Sequence of Strings.
//   * @tparam T the type of the resulting table.
//   * @return an IO[T]
//   */
//  def parseSequence[T: TableParser](wss: Iterator[Seq[String]]): IO[T] = {
//    val tableParser = implicitly[TableParser[T]]
//    tableParser match {
//      case parser: TableParserIO[T] => parser.parseIO(wss, 1)
//      case _ => IO.raiseError(ParserException(s"parseSequence method for Seq[Seq[String]] incompatible with tableParser: $tableParser"))
//    }
//  }

  /**
   * Method to parse a table from a File as a table of Seq[String].
   *
   * @param f                the file.
   * @param maybeFixedHeader an optional fixed header. If None (the default), we expect to find the header defined in the first line of the file.
   * @param forgiving        forcing (defaults to true). If true (the default) then an individual malformed row will not prevent subsequent rows being parsed.
   * @param codec            (implicit) the encoding.
   * @return an IO of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseFileRaw(f: File, predicate: Try[RawRow] => Boolean, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true)(implicit codec: Codec): IO[Table[RawRow]] = {
    implicit val z: TableParser[Table[RawRow]] = RawTableParser(predicate, maybeFixedHeader, forgiving)
    parseFile[Table[RawRow]](f)
  }

  /**
   * Method to parse a table from a File as a table of Seq[String].
   *
   * @param pathname the path name.
   * @param codec    (implicit) the encoding.
   * @return an IO of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseFileRaw(pathname: String, predicate: Try[RawRow] => Boolean)(implicit codec: Codec): IO[Table[RawRow]] = {
    implicit val z: TableParser[Table[RawRow]] = RawTableParser(predicate, None)
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
   * @return an IO of Table[RawRow] where RawRow is a Seq[String].
   */
  def parseResourceRaw(s: String, predicate: Try[RawRow] => Boolean = includeAll, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true, clazz: Class[_] = getClass)(implicit codec: Codec): IO[Table[RawRow]] = {
    implicit val z: TableParser[Table[RawRow]] = RawTableParser(predicate, maybeFixedHeader, forgiving)
    parseResource[Table[RawRow]](s, clazz)
  }

  /**
   * Method to parse a table of raw rows from an Iterable of String.
   *
   * @param ws the Strings.
   * @return an IO of Table of RawRow
   */
  def parseRaw(ws: Iterable[String], predicate: Try[RawRow] => Boolean = includeAll, maybeFixedHeader: Option[Header] = None, forgiving: Boolean = true, multiline: Boolean = true): IO[RawTable] = {
    implicit val z: TableParser[RawTable] = RawTableParser(predicate, maybeFixedHeader, forgiving, multiline)
    parse(ws.iterator, 0)
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
  def toCSVRow(t: Table[Row])(implicit csvAttributes: CsvAttributes, renderer: CsvRenderer[Row]): IO[String] = {
    t.maybeHeader match {
      case Some(hdr) =>
        implicit val z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        import Row.CsvRendererRow
        IO.fromTry(t.toCSV)
      case _ => throw TableException("toCSVRow: cannot write this Table to CSV (no header)")
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
  def writeCSVFileRow(t: Table[Row], file: File)(implicit csvAttributes: CsvAttributes): IO[FileWriter] =
    t.maybeHeader match {
      case Some(hdr) =>
        implicit val z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        // CONSIDER this is a bit ugly
        val q: IO[FileWriter] = t.writeCSVFileIO(file)
        q map { f => f.close(); f }
      case _ => throw TableException("writeCSVFileRow: cannot write this Table to CSV (no header)")
    }

  /**
   * Method to open source defined by an InputStream.
   *
   * @param s an InputStream.
   * @return an IO[Source].
   */
  private def sourceFromInputStream(s: => InputStream): IO[Source] = IO(Source.fromInputStream(s))

  /**
   * Method to open source defined by a URI.
   *
   * @param u a URI.
   * @return an IO[Source].
   */
  private def sourceFromURI(u: => URI): IO[Source] = IO(Source.fromURI(u))

  /**
   * Method to open source defined by a File.
   *
   * @param f a File.
   * @return an IO[Source].
   */
  private def sourceFromFile(f: => File)(implicit codec: Codec): IO[Source] = IO(Source.fromFile(f))

  /**
   * Method to open source defined by a File.
   *
   * @param filename a File.
   * @return an IO[Source].
   */
  private def sourceFromFilename(filename: => String)(implicit codec: Codec): IO[Source] = IO(Source.fromFile(filename))

  /**
   * Method to open a source defined by a Class and a resource name.
   *
   * @param w     the resource name.
   * @param clazz the class.
   * @return an IO[Source].
   */
  private def sourceFromClassResource(w: String, clazz: Class[_])(implicit codec: Codec): IO[Source] = IO.fromTry(Try(Source.fromURL(clazz.getResource(w))).recoverWith {
    case _: java.lang.NullPointerException => Failure(TableParserException(s"Table.sourceFromClassResource: cannot find resource '$w' relative to $clazz"))
  })
}
