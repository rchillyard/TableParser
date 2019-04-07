/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import java.io.{File, InputStream}
import java.net.{URI, URL}

import com.phasmidsoftware.parse.{ParserException, StringTableParser, StringsTableParser, TableParser}
import com.phasmidsoftware.render.{Renderer, Renderers, TreeWriter}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}

/**
  * A Table of Rows.
  *
  * @tparam Row the type of each row.
  */
trait Table[Row] extends Iterable[Row] {

	def maybeHeader: Option[Header]

	def map[S](f: Row => S): Table[S] = unit(rows map f, maybeHeader)

	def flatMap[U](f: Row => Table[U]): Table[U] = (rows map f).foldLeft(unit[U](Nil, None))(_ ++ _)

	def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S]

	def ++[U >: Row](table: Table[U]): Table[U] = unit[U](rows ++ table.rows, for (h1 <- maybeHeader; h2 <- table.maybeHeader) yield h1 ++ h2)

	def rows: Seq[Row]

	def iterator: Iterator[Row] = rows.iterator

	def render[U: TreeWriter](style: String)(implicit rr: Renderer[Row]): U = {
		object TableRenderers extends Renderers {
			val tableRenderer: Renderer[Seq[Row]] = sequenceRenderer[Row](style)
		}
		import TableRenderers._
		tableRenderer.render(rows, None)


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
    * CONSIDER closing the Source.
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
    *
    * @param i     the InputStream.
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](i: InputStream)(implicit codec: Codec): Try[T] = for (s <- Try(Source.fromInputStream(i)); t <- parse(s)) yield t

  /**
    * Method to parse a table from an File.
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

case class TableWithHeader[Row](rows: Seq[Row], header: Header) extends BaseTable[Row](rows, Some(header)) {
  override def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case Some(h) => TableWithHeader(rows, h);
    case _ => throw TableException("header is non-existent")
  }
}

case class TableWithoutHeader[Row](rows: Seq[Row]) extends BaseTable[Row](rows, None) {
  override def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match {
    case None => TableWithoutHeader(rows);
    case _ => throw TableException("header should be non-existent")
  }
}

case class TableException(w: String) extends Exception(w)