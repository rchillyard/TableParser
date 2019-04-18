/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import java.io.{File, InputStream}
import java.net.{URI, URL}

import com.phasmidsoftware.parse.{ParserException, StringTableParser, StringsTableParser, TableParser}
import com.phasmidsoftware.render._

import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}

/**
  * A Table of Rows.
  *
  * @tparam Row the type of each row.
  */
trait Table[Row] extends Iterable[Row] {

  /**
    * Method to retrieve the (optional) Header.
    *
    * @return the (optional) Header of this table.
    */
  def maybeHeader: Option[Header]

  /**
    * Transform (map) this Table[Row] into a Table[S].
    *
    * @param f a function which transforms a Row into an S.
    * @tparam S the type of the rows of the result.
    * @return a Table[S] where each cell has value f(x) where x is the value of the corresponding cell in this.
    */
  def map[S](f: Row => S): Table[S] = unit(rows map f, maybeHeader)

  /**
    * Transform (flatMap) this Table[Row] into a Table[S].
    *
    * @param f a function which transforms a Row into a Table[S].
    * @tparam S the type of the rows of the result.
    * @return a Table[S] where each cell has value f(x) where x is the value of the corresponding cell in this.
    */
  def flatMap[S](f: Row => Table[S]): Table[S] = (rows map f).foldLeft(unit[S](Nil, None))(_ ++ _)

  /**
    * Method to generate a Table[S] for a set of rows.
    * Although declared as an instance method, this method produces its result independent of this.
    *
    * @param rows        a sequence of S.
    * @param maybeHeader an (optional) header.
    * @tparam S the underlying type of the rows and the result.
    * @return a new instance of Table[S].
    */
  def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S]

  /**
    * Method to concatenate two Rows
    *
    * @param table a table to be concatenated with this table.
    * @tparam S the type of the rows of the result.
    * @return a new table, which is concatenated to this table, by rows.
    */
  def ++[S >: Row](table: Table[S]): Table[S] = unit[S](rows ++ table.rows, for (h1 <- maybeHeader; h2 <- table.maybeHeader) yield h1 ++ h2)

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

object Table {

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
      case _ => Failure(ParserException(s"parse method incompatible with tableParser: $tableParser"))
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
        x.close(); result
      } catch {
        case e: Exception => Failure(e)
      }
      case _ => result
    }
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
    * Method to parse a table from a URL.
    *
    * @param u the URI.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: URL)(implicit codec: Codec): Try[T] = for (uri <- Try(u.toURI); t <- parse(uri)) yield t

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
    *
    * @param s     the resource name.
    * @param clazz the class for which the resource should be sought (defaults to the calling class).
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseResource[T: TableParser](s: String, clazz: Class[_] = getClass)(implicit codec: Codec): Try[T] =
    clazz.getResource(s) match {
      case null => Failure(ParserException(s"Table.getResource: $s does not exist for $clazz"))
      case u => parse(u)
    }

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
      case _ => Failure(ParserException(s"parse method incompatible with tableParser: $tableParser"))
    }
  }

}

case class Header(xs: Seq[String]) {
  def getIndex(w: String): Int = xs.indexOf(w.toUpperCase)

  def ++(other: Header): Header = Header(xs ++ other.xs)
}

object Header {
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
}

/**
  * Concrete case class implementing BaseTable with a Header.
  *
  * CONSIDER merging the two cases.
  *
  * @param rows   the rows of the table.
  * @param header the header.
  * @tparam Row the underlying type of each Row
  */
case class TableWithHeader[Row](rows: Seq[Row], header: Header) extends BaseTable[Row](rows, Some(header)) {
  override def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case Some(h) => TableWithHeader(rows, h);
    case _ => throw TableException("header is non-existent")
  }
}

/**
  * Concrete case class implementing BaseTable without a Header.
  *
  * CONSIDER merging the two cases.
  *
  * @param rows the rows of the table.
  * @tparam Row the underlying type of each Row
  */
case class TableWithoutHeader[Row](rows: Seq[Row]) extends BaseTable[Row](rows, None) {
  override def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case None => TableWithoutHeader(rows);
    case _ => throw TableException("header should be non-existent")
  }
}

case class TableException(w: String) extends Exception(w)