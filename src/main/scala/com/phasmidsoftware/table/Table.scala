/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import java.io.{File, InputStream}
import java.net.{URI, URL}

import com.phasmidsoftware.parse.{ParserException, StringTableParser, StringsTableParser, TableParser}
import com.phasmidsoftware.render._
import com.phasmidsoftware.util.{FP, Reflection}

import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * A Table of Rows.
  *
  * @tparam Row the type of each row.
  */
trait Table[Row] extends Iterable[Row] with Renderable[Row] {

  /**
    * Optional value of the Header of this Table, if there is one.
    */
  val maybeHeader: Option[Header]

  /**
    * Transform (map) this Table[Row] into a Table[S].
    *
    * @param f a function which transforms a Row into an S.
    * @tparam S the type of the rows of the result.
    * @return a Table[S] where each cell has value f(x) where x is the value of the corresponding cell in this.
    */
  override def map[S](f: Row => S): Table[S] = unit(rows map f)

  /**
    * Transform (flatMap) this Table[Row] into a Table[S].
    *
    * @param f a function which transforms a Row into a Table[S].
    * @tparam S the type of the rows of the result.
    * @return a Table[S] where each cell has value f(x) where x is the value of the corresponding cell in this.
    */
  def flatMap[S](f: Row => Table[S]): Table[S] = (rows map f).foldLeft(unit[S](Nil))(_ ++ _)

  /**
    * Method to concatenate two Rows
    *
    * @param table a table to be concatenated with this table.
    * @tparam S the type of the rows of the result.
    * @return a new table, which is concatenated to this table, by rows.
    */
  def ++[S >: Row](table: Table[S]): Table[S] = unit[S](rows ++ table.rows)

  /**
    * Method to generate a Table[S] for a set of rows.
    * Although declared as an instance method, this method produces its result independent of this.
    *
    * @param rows a sequence of S.
    * @tparam S the underlying type of the rows and the result.
    * @return a new instance of Table[S].
    */
  def unit[S](rows: Seq[S]): Table[S]

  /**
    * Method to access the individual rows of this table.
    *
    * @return the rows, in the same sequence in which they were parsed.
    */
  def rows: Seq[Row]

  /**
    * Method to return the rows of this table as an iterator.
    *
    * @return the rows in the form of Iterator[Row]
    */
  def iterator: Iterator[Row] = rows.iterator
}

object Table {

  /**
    * Method to parse a table from a URI with an explicit encoding.
    *
    * @param u   the URI.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: URI, enc: String): Try[T] = {
    implicit val codec: Codec = Codec(enc)
    parse(u)
  }

  /**
    * Method to parse a table from a URI with an implicit encoding.
    *
    * @param u     the URI.
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: URI)(implicit codec: Codec): Try[T] = for (s <- Try(Source.fromURI(u)); t <- parse(s)) yield t

  /**
    * Method to parse a table from a URL with an explicit encoding.
    * NOTE: The source created will be closed by the parse method.
    *
    * @param u   the URL.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: URL, enc: String): Try[T] = for (s <- Try(Source.fromURL(u, enc)); t <- parse(s)) yield t

  /**
    * Method to parse a table from an InputStream with an explicit encoding.
    *
    * @param i   the InputStream.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](i: InputStream, enc: String): Try[T] = {
    implicit val codec: Codec = Codec(enc)
    parse(i)
  }

  /**
    * Method to parse a table from an InputStream with an implicit encoding.
    * NOTE: The source created will be closed by the parse method.
    *
    * @param i     the InputStream.
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](i: InputStream)(implicit codec: Codec): Try[T] = for (s <- Try(Source.fromInputStream(i)); t <- parse(s)) yield t

  /**
    * Method to parse a table from a Source.
    * The Source will close itself after extracting the lines.
    *
    * @param x the Source.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](x: Source): Try[T] = {
    val result = parse(x.getLines())
    result match {
      case Success(_) => try {
        x.close()
        result
      } catch {
        case NonFatal(e) => Failure(e)
      }
      case _ => result
    }
  }

  /**
    * Method to parse a table from an Iterator of String.
    *
    * @param ws the iterator.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](ws: Iterator[String]): Try[T] = parse(ws.toSeq)

  /**
    * Method to parse a table from a Seq of String.
    *
    * @param ws the Strings.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](ws: Seq[String]): Try[T] = {
    val tableParser = implicitly[TableParser[T]]
    tableParser match {
      case parser: StringTableParser[T] => parser.parse(ws)
      case _ => Failure(ParserException(s"parse method for Seq[String] incompatible with tableParser: $tableParser"))
    }
  }

  /**
    * Method to parse a table from a File.
    *
    * @param f   the File.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](f: File, enc: String): Try[T] = {
    implicit val codec: Codec = Codec(enc)
    parse(f)
  }

  /**
    * Method to parse a table from an File.
    * NOTE: The source created will be closed by the parse method.
    *
    * @param f     the File.
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](f: File)(implicit codec: Codec): Try[T] = for (s <- Try(Source.fromFile(f)); t <- parse(s)) yield t

  /**
    * Method to parse a table from an File.
    *
    * @param s     the resource name.
    * @param clazz the class for which the resource should be sought (defaults to the calling class).
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseResource[T: TableParser](s: String, clazz: Class[_] = getClass)(implicit codec: Codec): Try[T] =
    Option(clazz.getResource(s)) match {
      case None => Failure(ParserException(s"Table.getResource: $s does not exist for $clazz"))
      case Some(u) => parse(u)
    }

  /**
    * Method to parse a table from a URL.
    *
    * @param u the URI.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: => URL)(implicit codec: Codec): Try[T] =
    for (uri <- Try(u.toURI); t <- parse(uri)) yield t

  /**
    * Method to parse a table from a Seq of Seq of String.
    *
    * @param wss the Sequence of Strings.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseSequence[T: TableParser](wss: Seq[Seq[String]]): Try[T] = {
    val tableParser = implicitly[TableParser[T]]
    tableParser match {
      case parser: StringsTableParser[T] => parser.parse(wss)
      case _ => Failure(ParserException(s"parse method for Seq[Seq[String]] incompatible with tableParser: $tableParser"))
    }
  }
}

/**
  * Case class to represent a header.
  *
  * @param xs the sequence of column names.
  */
case class Header(xs: Seq[String]) {
  def exists: Boolean = xs match {
    case _ :: _ => true
    case Nil => false
  }

  def getIndex(w: String): Try[Int] = FP.indexFound(w, xs.indexOf(w.toUpperCase))

  def ++(other: Header): Header = Header(xs ++ other.xs)
}

object Header {

  // TODO come back and figure out why recursiveLetters (below) didn't work properly.
  lazy val numbers: LazyList[Int] = LazyList.from(1)
  lazy val generateNumbers: LazyList[String] = numbers map (_.toString)
  //noinspection SpellCheckingInspection
  //  lazy val recursiveLetters: Stream[String] = alphabet.toStream #::: multiply(alphabet,recursiveLetters)
  //  lazy val generateLetters: Stream[String] = recursiveLetters
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
    val wss: List[LazyList[String]] = prefixes map (prepend(_, strings))
    wss.foldLeft(LazyList.empty[String])(_ #::: _)
  }

  def prepend(prefix: String, stream: LazyList[String]): LazyList[String] = stream map (prefix + _)

  /**
    * This method constructs a new Header based on Excel row/column names.
    *
    * @param letters true if we want the sequence A B C D E ... Z AA AB ... BA BB ...
    *                false is we just want numbers.
    * @return a
    */
  def apply(letters: Boolean, length: Int): Header = Header(numbers map intToString(letters) take length toList)

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
  def apply[X: ClassTag](): Header = Header(Reflection.extractFieldNames(implicitly[ClassTag[X]]).toList)

  def create(ws: String*): Header = apply(ws map (_.toUpperCase))
}



/**
  * CONSIDER eliminating this base class
  *
  * @param rows        the rows of the table
  * @param maybeHeader (optional) header
  * @tparam Row the underlying type of each Row
  */
abstract class BaseTable[Row](rows: Seq[Row], val maybeHeader: Option[Header]) extends Table[Row] {
  self =>

  /**
    * Method to render a table in a sequential (serialized) fashion.
    *
    * @tparam O a type which supports Writable (via evidence of type Writable[O])
    * @return a new (or possibly old) instance of O.
    */
  def render[O: Writable]: O = {
    val ww = implicitly[Writable[O]]
    val o1 = ww.unit
    val o2 = (maybeHeader map (h => ww.writeRaw(ww.writeRowElements(o1)(h.xs))(ww.newline))).getOrElse(o1)
    rows map {
      case p: Product => ww.writeRow(o2)(p)
      case xs: Seq[Any] => ww.writeRowElements(o2)(xs)
      case xs: Array[Any] => ww.writeRowElements(o2)(xs)
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
    * @param rr         an (implicit) Renderer[Row]
    * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
    * @return a new instance of U which represents this Table as a tree of some sort.
    */
  def render[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Row]): U = {
    object TableRenderers extends Renderers {
      val rowsRenderer: Renderer[Seq[Row]] = sequenceRenderer[Row]("tbody")
      implicit val headerRenderer: Renderer[Header] = headerRenderer("tr", sequenced = false)(renderer("th", Map()))
      implicit val optionRenderer: Renderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    }
    import TableRenderers._
    val node: Node = implicitly[Renderer[Option[Header]]].render(maybeHeader)
    implicitly[TreeWriter[U]].evaluate(Node(style, attributes, node +: Seq(rowsRenderer.render(rows))))
  }

  /**
    * Method to render a table in a hierarchical fashion.
    *
    * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
    * outputting each row as you go.
    *
    * @param style      the "style" to be used for the node which will represent this table.
    * @param attributes the attributes to be applied to the top level node for this table.
    * @param rr         an (implicit) Renderer[ Indexed [ Row ] ]
    * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
    * @return a new instance of U which represents this Table as a tree of some sort.
    */
  def renderSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Indexed[Row]]): U = {
    object TableRenderers extends Renderers {
      val rowsRenderer: Renderer[Seq[Indexed[Row]]] = sequenceRenderer[Indexed[Row]]("tbody")
      implicit val headerRenderer: Renderer[Header] = headerRenderer("tr", sequenced = true)(renderer("th", Map()))
      implicit val optionRenderer: Renderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    }
    import TableRenderers._
    val headerNode: Node = implicitly[Renderer[Option[Header]]].render(maybeHeader)
    val tableNode = Node(style, attributes, headerNode +: Seq(rowsRenderer.render(Indexed.index(rows))))
    val trimmed = tableNode.trim
    implicitly[TreeWriter[U]].evaluate(trimmed)
  }
}

/**
  * Concrete case class implementing BaseTable without a Header.
  *
  * NOTE: the existence or not of a Header in a BaseTable only affects how the table is rendered.
  * The parsing of a table always has a header of some sort.
  *
  * @param rows the rows of the table.
  * @tparam Row the underlying type of each Row
  */
case class TableWithoutHeader[Row](rows: Seq[Row]) extends BaseTable[Row](rows, None) {
  def unit[S](rows: Seq[S]): Table[S] = TableWithoutHeader(rows)
}

/**
  * Concrete case class implementing BaseTable with a Header.
  *
  * NOTE: the existence or not of a Header in a BaseTable only affects how the table is rendered.
  * The parsing of a table always has a header of some sort.
  *
  * @param rows   the rows of the table.
  * @param header the header.
  * @tparam Row the underlying type of each Row
  */
case class TableWithHeader[Row](rows: Seq[Row], header: Header) extends BaseTable[Row](rows, Some(header)) {
  def unit[S](rows: Seq[S]): Table[S] = TableWithHeader(rows, header)
}

object TableWithHeader {
  def apply[Row: ClassTag](rows: Seq[Row]): Table[Row] = TableWithHeader(rows, Header.apply[Row]())
}

case class TableException(w: String) extends Exception(w)
