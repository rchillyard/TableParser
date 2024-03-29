/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import cats.effect.IO
import com.phasmidsoftware.crypto.HexEncryption
import com.phasmidsoftware.parse.TableParser.includeAll
import com.phasmidsoftware.parse._
import com.phasmidsoftware.render._
import com.phasmidsoftware.util.FP._
import com.phasmidsoftware.util.{IOUsing, Reflection}
import com.phasmidsoftware.write.{Node, TreeWriter, Writable}
import java.io.{File, FileWriter, InputStream}
import java.net.{URI, URL}
import scala.annotation.unused
import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.{Failure, Random, Try}

/**
 * A Table of Content.
 *
 * @tparam Row the type of each row.
 */
trait Table[Row] extends Iterable[Row] {

  /**
   * Optional value of the Header of this Table, if there is one.
   */
  val maybeHeader: Option[Header]

  /**
   * Method to clone this Table but with a different Header.
   *
   * @param ho an optional Header.
   * @return a Table[Row] with the same rows as this, but with ho as its maybeHeader.
   */
  def replaceHeader(ho: Option[Header]): Table[Row] = unit(content, ho)

  /**
   * Transform (map) this Table[Row] into a Table[S].
   *
   * @param f a function which transforms a Row into an S.
   * @tparam S the type of the rows of the result.
   * @return a Table[S] where each row has value f(x) where x is the value of the corresponding row in this.
   */
  override def map[S](f: Row => S): Table[S] = unit[S](content map f, maybeHeader)

  /**
   * Transform (flatMap) this Table[Row] into a Table[S].
   *
   * CONSIDER rewriting this method or redefining it as it can be a major source of inefficiency.
   * In particular, we are doing a ++ operation, which is inherently bad.
   *
   * @param f a function which transforms a Row into an IterableOnce[S].
   * @tparam S the type of the rows of the result.
   * @return a Table[S] which is made up of a concatenation of the results of invoking f on each row this
   */
  def flatMap[S](f: Row => Iterable[S]): Table[S] = (content map f).foldLeft(unit[S](Nil))((a, e) => a ++ unit(e))

  /**
   * Transform (flatMap) this Table[Row] into a Table[S].
   *
   * @param f a function which transforms a Row into an IterableOnce[S].
   * @tparam S the type of the rows of the result.
   * @return a Table[S] which is made up of a concatenation of the results of invoking f on each row this
   */
  def mapOptional[S](f: Row => Option[S]): Table[S] = unit[S](content.mapOptional(f), maybeHeader)

  /**
   * Method to zip two Tables together such that the rows of the resulting table are tuples of the rows of the input tables.
   *
   * @param rt a Table[R].
   * @tparam R the underlying type of the other Table.
   * @return a Table of (Row, R).
   */
  def zip[R](rt: Table[R]): Table[(Row, R)] = doZip[R, (Row, R)]((rs1, rs2) => rs1.toSeq zip rs2.toSeq)(rt)

  /**
   * Method to concatenate two Content
   *
   * @param table a table to be concatenated with this table.
   * @tparam S the type of the rows of the result.
   * @return a new table, which is concatenated to this table, by rows.
   */
  def ++[S >: Row](table: Table[S]): Table[S] = unit[S](content ++ table.content, maybeHeader)

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param sr          a Content of S.
   * @param maybeHeader an optional Header to be used in the resulting Table.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  def unit[S](sr: Content[S], maybeHeader: Option[Header]): Table[S]

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param ss          a sequence of S.
   * @param maybeHeader an optional Header to be used in the resulting Table.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  def unit[S](ss: Iterable[S], maybeHeader: Option[Header]): Table[S]

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param ss a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  def unit[S](ss: Iterable[S]): Table[S] = unit(ss, maybeHeader)

  /**
   * Method to access the individual rows of this table.
   *
   * @return the rows, in the same sequence in which they were parsed.
   */
  def content: Content[Row]

  /**
   * Method to select those rows defined by the given range.
   * NOTE: the rows are numbered 1..N.
   *
   * @param range a Range
   * @return a new Table[Row] consisting only of those rows in the given range.
   */
  def select(range: Range): Table[Row] = {
    val ys: IndexedSeq[Row] = asOneBasedIndexedSequence
    Table(for (i <- range) yield ys(i), maybeHeader)
  }

  /**
   * Method to select those rows defined by the given range.
   * NOTE: the rows are numbered 1..N.
   *
   * @param n the desired row.
   * @return a new Table[Row] consisting only the row requested.
   */
  def select(n: Int): Table[Row] = processRows(_.slice(n - 1, n))

  /**
   * Method to yield a Seq of the rows of this Table.
   *
   * @return a Seq[Row]
   */
  override def toSeq: Seq[Row] = {
    // XXX huh?
    lazy val rs = content.toSeq
    rs
  }

  /**
   * Method to yield an Array from the rows of this Table.
   *
   * @tparam Element the element type of the resulting Array.
   * @return an Array[Element].
   */
  override def toArray[Element >: Row : ClassTag]: Array[Element] = {
    // XXX huh?
    lazy val rs = content.toArray[Element]
    rs
  }

  /**
   * Method to return the rows of this table as an iterator.
   *
   * @return the rows in the form of Iterator[Row]
   */
  def iterator: Iterator[Row] = content.iterator

  /**
   * Method to process the rows as an Iterable into an Iterable which will make up the resulting Table.
   * NOTE: if you need the rows processed individually, use processRowsMap or flatMap.
   *
   * @param f a function which takes an Iterable[Row] and returns an Iterable[S]
   * @tparam S the underlying type of the result.
   * @return a table[S]
   */
  def processRows[S](f: Content[Row] => Content[S]): Table[S] = unit(f(content), maybeHeader)

  /**
   * Method to process the rows as an Iterable into an Iterable which will make up the resulting Table.
   *
   * TESTME
   *
   * @param f a function which takes an Iterable[Row] and returns an Iterable[S]
   * @tparam S the underlying type of the result.
   * @return a table[S]
   */
  @unused
  def processRowsMap[S](f: Row => S): Table[S] = unit(content.map(f), maybeHeader)

  /**
   * Method to transform this Table[Row] into a sorted Table[S] where S is a super-class of Row and for which there is
   * evidence of Ordering[S].
   *
   * @tparam S the underlying type of the resulting Table (a super-type of Row and for which there is evidence of Ordering[S]).
   * @return a Table[S].
   */
  def sort[S >: Row : Ordering]: Table[S] = unit[S](content.sorted[S], maybeHeader)

  /**
   * Method to shuffle this Table[Row].
   *
   * @return a Table[Row].
   */
  lazy val shuffle: Table[Row] = processRows(rs => Content(Random.shuffle(rs.toSeq)))

  /**
   * drop (as defined by Iterable).
   *
   * @param n the number of rows to drop.
   * @return a Table like this Table but without its first n rows.
   */
  override def drop(n: Int): Table[Row] = processRows(_.drop(n))

  /**
   * dropWhile (as defined by Iterable).
   *
   * @param p the predicate.
   * @return a Table like this Table but with dropWhile(p) rows.
   */
  override def dropWhile(p: Row => Boolean): Table[Row] = processRows(_.dropWhile(p))

  /**
   * Method to return an empty Table of type Row.
   *
   * @return a Table[Row] without any rows.
   */
  override def empty: Table[Row] = unit(Seq.empty)

  /**
   * Method to filter the rows of a table (as defined by Iterable).
   *
   * @param p a predicate to be applied to each row.
   * @return a Table[Row] consisting only of rows which satisfy the predicate p.
   */
  override def filter(p: Row => Boolean): Table[Row] = processRows(_.filter(p))

  /**
   * Method to filter out the rows of a table (as defined by Iterable).
   *
   * @param p a predicate to be applied to each row.
   * @return a Table[Row] consisting only of rows which do not satisfy the predicate p.
   */
  override def filterNot(p: Row => Boolean): Table[Row] = processRows(_.filterNot(p))

  /**
   * slice (as defined by Iterable).
   *
   * @param from  the index at which to begin the slice (1-based counting).
   * @param until the index at which to end the slice (1-based counting).
   * @return a Table like this Table but with slice(from, until) rows.
   */
  override def slice(from: Int, until: Int): Table[Row] = processRows(_.slice(from, until))

  /**
   * take (as defined by Iterable).
   *
   * @param n the number of rows to take.
   * @return a Table like this Table but with only its first n rows.
   */
  override def take(n: Int): Table[Row] = processRows(_.take(n))

  /**
   * takeWhile (as defined by Iterable).
   *
   * @param p the predicate.
   * @return a Table like this Table but with takeWhile(p) rows.
   */
  override def takeWhile(p: Row => Boolean): Table[Row] = processRows(_.takeWhile(p))

  /**
   * Filter method which operates on the (primary) key of each row.
   *
   * TESTME (probably tested only in it)
   *
   * @param p a predicate which takes a String.
   * @tparam T a super-class of Row, which provides evidence of HasKey[T].
   * @return a filtered Table[Row].
   */
  def filterByKey[T >: Row : HasKey](p: String => Boolean): Table[Row] =
    filter(t => p(implicitly[HasKey[T]].key(t)))

  /**
   * Filter method which operates on the (primary) key of each row.
   *
   * @param p a predicate which takes a String.
   * @tparam T a super-class of Row, which provides evidence of HasKey[T].
   * @return a filtered Table[Row].
   */
  def filterNotByKey[T >: Row : HasKey](p: String => Boolean): Table[Row] =
    filterNot(t => p(implicitly[HasKey[T]].key(t)))

  /**
   * Method to render this Table[T] as a CSV String with (maybe) header.
   *
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   * @return a String.
   */
  def toCSV(implicit renderer: CsvRenderer[Row], generator: CsvGenerator[Row], csvAttributes: CsvAttributes): IO[String] =
    CsvTableStringRenderer[Row]().render(this) map (_.toString)

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   *
   * @param file      instance of File where the output should be stored.
   * @param renderer  implicit value of CsvRenderer[Row].
   * @param generator implicit value of CsvProductGenerator[Row].
   * @param hasKey    implicit value of HasKey[Row].
   *                  This relates to a column which is the "key" column in a CSV (used for identification).
   *                  It is not directly related to cryptography.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @param csvAttributes implicit value of CsvAttributes.
   */
  def writeCSVFileEncrypted[A: HexEncryption](file: File)(implicit renderer: CsvRenderer[Row], generator: CsvGenerator[Row], hasKey: HasKey[Row], csvAttributes: CsvAttributes): Unit =
    CsvTableEncryptedFileRenderer[Row, A](file).render(this)

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   *
   * TESTME (probably tested only in it)
   *
   * @param file          instance of File where the output should be stored.
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   */
  def writeCSVFile(file: File)(implicit renderer: CsvRenderer[Row], generator: CsvGenerator[Row], csvAttributes: CsvAttributes): Unit =
    CsvTableFileRenderer[Row](file).render(this)

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   *
   * @param file          instance of File where the output should be stored.
   * @param renderer      implicit value of CsvRenderer[Row].
   * @param generator     implicit value of CsvProductGenerator[Row].
   * @param csvAttributes implicit value of CsvAttributes.
   */
  def writeCSVFileIO(file: File)(implicit renderer: CsvRenderer[Row], generator: CsvGenerator[Row], csvAttributes: CsvAttributes): IO[FileWriter] =
    CsvTableFileRenderer[Row](file).render(this) map { f => f.flush(); f }

  def maybeColumnNames: Option[Seq[String]] = maybeHeader map (_.xs)

  /**
   * Method to get all the elements of a particular column.
   *
   * Complexity: N + C where N is the number of rows and C is the number of columns.
   *
   * @param name the column name.
   * @return an Iterator of Option[String].
   */
  def column(name: String): Iterator[Option[String]]

  /**
   * Method to process the rows of this Table and the other Table as a pair of Iterables resulting in an Iterable which will make up the resulting Table.
   *
   * @param f     a function which takes an Iterable[Row] and an Iterable[R] and returns an Iterable[S]
   * @param other the other table of type Iterable[R].
   * @tparam R the underlying type of the other table.
   * @tparam S the underlying type of the result.
   * @return a table[S]
   */
  private def doZip[R, S](f: (Iterable[Row], Iterable[R]) => Iterable[S])(other: Table[R]): Table[S] = unit(f(content.toSeq, other.content.toSeq))

  private lazy val asOneBasedIndexedSequence = new IndexedSeq[Row]() {
    def apply(i: Int): Row = content.toIndexedSeq(i - 1)

    // TESTME
    def length: Int = content.size
  }
}

object Table {
  /**
   * Primary method to parse a table from an Iterator of String.
   * This method is, in turn, invoked by all other parse methods defined below (other than parseSequence).
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterator[String]): IO[T] = implicitly[TableParser[T]] match {
    case parser: StringTableParser[T] => parser.parse(ws, parser.headerRowsToRead)
    case x => IO.raiseError(ParserException(s"parse method for Seq[String] incompatible with tableParser: $x"))
  }

  /**
   * Method to parse a table from an Iterable of String.
   *
   * @param ws the Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parse[T: TableParser](ws: Iterable[String]): IO[T] = parse(ws.iterator)

  /**
   * Method to parse a table from a Source.
   *
   * NOTE: the caller is responsible for closing the given Source.
   *
   * @param x the Source.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseSource[T: TableParser](x: => Source): IO[T] = for (y <- parse(x.getLines())) yield y

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

  /**
   * Method to parse a table from a Seq of Seq of String.
   *
   * @param wss the Sequence of Strings.
   * @tparam T the type of the resulting table.
   * @return an IO[T]
   */
  def parseSequence[T: TableParser](wss: Iterator[Seq[String]]): IO[T] = {
    val tableParser = implicitly[TableParser[T]]
    tableParser match {
      case parser: StringsTableParser[T] => parser.parse(wss, 1)
      case _ => IO.raiseError(ParserException(s"parseSequence method for Seq[Seq[String]] incompatible with tableParser: $tableParser"))
    }
  }

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
  def toCSVRow(t: Table[Row])(implicit csvAttributes: CsvAttributes): IO[String] = {
    t.maybeHeader match {
      case Some(hdr) =>
        implicit val z: CsvGenerator[Row] = Row.csvGenerator(hdr)
        t.toCSV
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

/**
 * CONSIDER eliminating this base class or, perhaps, rename it.
 *
 * @param rows        the rows of the table
 * @param maybeHeader (optional) header
 * @tparam Row the underlying type of each Row
 */
abstract class RenderableTable[Row](rows: Content[Row], val maybeHeader: Option[Header]) extends Table[Row] with TableRenderable[Row] {
  self =>

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param sr a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  override def unit[S](sr: Content[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case Some(h) => HeadedTable(sr, h)
    case None => UnheadedTable(sr)
  }

  /**
   *
   * @param ev implicit evidence for Renderer of Table of X.
   * @tparam O the type of the result.
   * @return an instance of O.
   */
  def render[O](implicit ev: Renderer[Table[Row], O]): O = ev.render(this)

  /**
   * CONSIDER redefining the definition of Renderer such that we can accommodate the various different types of output.
   *
   * Method to render a table in a sequential (serialized) fashion.
   *
   * @tparam O a type which supports Writable (via evidence of type Writable[O])
   * @return a new (or possibly old) instance of O.
   */
  def renderToWritable[O: Writable]: O = {
    val ww = implicitly[Writable[O]]
    val o1 = ww.unit
    val o2 = (maybeHeader map (h => ww.writeRaw(ww.writeRowElements(o1)(h.xs))(ww.newline))).getOrElse(o1)
    val knownSize1 = rows.knownSize
    // TODO this makes no sense now: the decision is taken inside Content.
    (if (knownSize1 > -1) rows.toSeq else rows.toSeq) map {
      case p: Product => ww.writeRow(o2)(p)
      case xs: Seq[Row] => ww.writeRowElements(o2)(xs) // TESTME
      case xs: Array[Row] => ww.writeRowElements(o2)(xs.toIndexedSeq) // TESTME
      case _ => throw TableException("cannot render table because row is neither a Product, nor an array nor a sequence")
    }
    o1
  }

  /**
   * Method to render a table in a hierarchical fashion.
   *
   * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
   * outputting each row as you go.
   *
   * @param style      the "style" to be used for the node which will represent this table.
   * @param attributes the attributes to be applied to the top level node for this table.
   * @param xr         an (implicit) HierarchicalRenderer[Row]
   * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
   * @return a new instance of U which represents this Table as a tree of some sort.
   */
  def renderHierarchical[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit xr: HierarchicalRenderer[Row]): U = {
    object TableRenderers extends HierarchicalRenderers {
      val rowsRenderer: HierarchicalRenderer[Seq[Row]] = sequenceRenderer[Row]("tbody")
      implicit val headerRenderer: HierarchicalRenderer[Header] = headerRenderer("tr", sequenced = false)(renderer("th", Map()))
      implicit val optionRenderer: HierarchicalRenderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    }
    import TableRenderers._
    val node: Node = implicitly[HierarchicalRenderer[Option[Header]]].render(maybeHeader)
    implicitly[TreeWriter[U]].evaluate(Node(style, attributes, node +: Seq(rowsRenderer.render(rows.toSeq))))
  }

  /**
   * Method to render a table in a hierarchical fashion.
   *
   * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
   * outputting each row as you go.
   *
   * @param style      the "style" to be used for the node which will represent this table.
   * @param attributes the attributes to be applied to the top level node for this table.
   * @param rr         an (implicit) HierarchicalRenderer[ Indexed [ Row ] ]
   * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
   * @return a new instance of U which represents this Table as a tree of some sort.
   */
  def renderHierarchicalSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: HierarchicalRenderer[Indexed[Row]]): U = {
    object TableRenderers extends HierarchicalRenderers {
      val rowsRenderer: HierarchicalRenderer[Seq[Indexed[Row]]] = sequenceRenderer[Indexed[Row]]("tbody")
      implicit val headerRenderer: HierarchicalRenderer[Header] = headerRenderer("tr", sequenced = true)(renderer("th", Map()))
      implicit val optionRenderer: HierarchicalRenderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    }
    import TableRenderers._
    val headerNode: Node = implicitly[HierarchicalRenderer[Option[Header]]].render(maybeHeader)
    val tableNode = Node(style, attributes, headerNode +: Seq(rowsRenderer.render(Indexed.index(rows.toSeq))))
    val trimmed = tableNode.trim
    implicitly[TreeWriter[U]].evaluate(trimmed)
  }

  override def toString(): String = s"RenderableTable: header=$maybeHeader, with $size rows"
}

/**
 * Concrete case class implementing RenderableTable without a Header.
 *
 * NOTE: the existence or not of a Header in a RenderableTable only affects how the table is rendered.
 * The parsing of a table always has a header of some sort.
 *
 * TESTME this: it is currently not used.
 *
 * @param content the rows of the table.
 * @tparam Row the underlying type of each Row
 */
case class UnheadedTable[Row](content: Content[Row]) extends RenderableTable[Row](content, None) {
  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param ss a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  override def unit[S](ss: Iterable[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case Some(h) => HeadedTable(Content(ss), h)
    case None => UnheadedTable(Content(ss))
  }

  def column(name: String): Iterator[Option[String]] = Iterator.empty
}

object UnheadedTable {

  def apply[Row: ClassTag](rows: Iterable[Row]): Table[Row] = new UnheadedTable(Content(rows))

  def apply[Row: ClassTag](rows: Iterator[Row]): Table[Row] = UnheadedTable(rows.to(List))

}

/**
 * Concrete case class implementing RenderableTable with a Header.
 * The unit and apply methods are such that rows is in fact an Array[Row] (??).
 *
 * NOTE: the existence or not of a Header in a RenderableTable only affects how the table is rendered.
 * The parsing of a table always has a header of some sort.
 *
 * @param content the rows of the table, stored as an Array.
 * @param header  the header.
 * @tparam Row the underlying type of each Row
 */
case class HeadedTable[Row](content: Content[Row], header: Header) extends RenderableTable[Row](content, Some(header)) {

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param ss a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  override def unit[S](ss: Iterable[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case Some(h) => HeadedTable(Content(ss), h)
    case None => UnheadedTable(Content(ss))
  }

  /**
   * Method to get all the elements of a particular column.
   *
   * Complexity: N + C where N is the number of rows and C is the number of columns.
   *
   * @param name the column name.
   * @return an Iterator of Option[String].
   */
  def column(name: String): Iterator[Option[String]] = {
    val maybeIndex = maybeColumnNames map (_.indexOf(name))
    content.iterator map {
      case row: RawRow => maybeIndex map (row.ws(_))
      case ws: Seq[Any] => maybeIndex map (ws(_).toString)
      case _ => None
    }
  }

  override def toString(): String = s"HeadedTable($header) with ${content.size} rows"
}

/**
 * Companion object for HeadedTable.
 * The apply methods provided arbitrarily use Vector as the collection for the rows of the table.
 * CONSIDER using something else such as Array.
 *
 * TESTME ?
 */
object HeadedTable {

  def apply[Row: ClassTag](rows: Iterable[Row], header: Header): RenderableTable[Row] = new HeadedTable(Content(rows), header)

  def apply[Row: ClassTag](rows: Iterable[Row]): RenderableTable[Row] = HeadedTable(rows, Header[Row]())

  def apply[Row: ClassTag](rows: Iterator[Row], header: Header): RenderableTable[Row] = HeadedTable(rows.to(List), header)

  def apply[Row: ClassTag](rows: Iterator[Row]): RenderableTable[Row] = HeadedTable(rows, Header[Row]())

}

/**
 * Case class to represent a header.
 *
 * @param xs  the sequence of column names.
 * @param xss a sequence of sequence of String representing any additional header lines.
 */
case class Header(xs: Seq[String], xss: Seq[Seq[String]]) {
  /**
   * Get the number of columns.
   *
   * @return the size of the header sequence.
   */
  def size: Int = xs.length

  /**
   * Get the index of w in this Header, ignoring case.
   *
   * @param w the String to find, a column name.
   * @return the index wrapped in Try.
   */
  def getIndex(w: String): Try[Int] = indexFound(w, xs.indexWhere(x => x.compareToIgnoreCase(w) == 0))

  /**
   * Concatenate this Header with other.
   *
   * @param other the other Header.
   * @return a Header made up of these columns and those of other, in that order.
   */
  def ++(other: Header): Header = Header(xs ++ other.xs, for (xs <- xss; ys <- other.xss) yield xs ++ ys)
}

object Header {

  // TODO come back and figure out why recursiveLetters (below) didn't work properly.
  private lazy val numbers: LazyList[Int] = LazyList.from(1)
  lazy val generateNumbers: LazyList[String] = numbers map (_.toString)
  //  lazy val recursiveLetters: LazyList[String] = alphabet.toStream #::: multiply(alphabet,recursiveLetters)
  //  lazy val generateLetters: LazyList[String] = recursiveLetters
  val alphabet: List[String] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray.map(_.toString).toList

  private def intToString(letters: Boolean)(n: Int): String = if (letters) {
    @scala.annotation.tailrec
    def inner(s: String, n: Int): String = {
      if (n <= 0) s
      else {
        val m = n % 26 match {
          case 0 => 26
          case other => other
        }
        inner(s"${(m + 64).toChar}$s", (n - m) / 26)
      }
    }

    inner("", n)
  }
  else n.toString

  lazy val generateLetters: LazyList[String] = numbers map intToString(letters = true)

  def multiply(prefixes: List[String], strings: LazyList[String]): LazyList[String] = {
    prefixes collect {
      case "x" => "x"
    }
    val wss: List[LazyList[String]] = prefixes map (prepend(_, strings))
    wss.foldLeft(LazyList.empty[String])(_ #::: _)
  }

  def prepend(prefix: String, stream: LazyList[String]): LazyList[String] = stream map (prefix + _)

  /**
   * This method constructs a new Header based on a header spanning several lines.
   * Only the first becomes the true header, the remainder become the "preface."
   *
   * @param wss a sequence of sequence of String.
   * @return a Header.
   */
  def apply(wss: Seq[Seq[String]]): Header = apply(wss.head, wss.tail)

  /**
   * This method constructs a new Header based on Excel row/column names.
   *
   * @param letters true if we want the sequence A B C D E ... Z AA AB ... BA BB ...
   *                false is we just want numbers.
   * @return a
   */
  def apply(letters: Boolean, length: Int): Header = Header(numbers map intToString(letters) take length toList, Nil)

  /**
   * This method constructs a new Header based on the fields of the class X.
   * It should not be considered to be the normal method of generating the header for a table.
   * It is mainly used by unit tests.
   *
   * NOTE: this method does NOT recurse into any fields which happen also to be a class with fields.
   *
   * @tparam X a class X which is not necessarily a Product.
   * @return a List of field names.
   */
  def apply[X: ClassTag](): Header = Header(Reflection.extractFieldNames(implicitly[ClassTag[X]]).toList, Nil)

  /**
   * Create a Header from a variable list of parameters.
   *
   * @param ws a variable list of Strings.
   * @return a Header.
   */
  def create(ws: String*): Header = apply(ws, Nil)
}

/**
 * This typeclass pertains to a table row type T that has one particular column which is considered
 * the primary key (row-id, identifier, whatever).
 *
 * @tparam Row the underlying type.
 */
trait HasKey[Row] {
  def key(t: Row): String
}

/**
 * Table Exception.
 *
 * @param w the message.
 */
case class TableException(w: String) extends Exception(w)


