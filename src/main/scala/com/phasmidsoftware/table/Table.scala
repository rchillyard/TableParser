/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import java.io.{File, InputStream}
import java.net.{URI, URL}

import com.phasmidsoftware.parse.{ParserException, StringTableParser, StringsTableParser, TableParser}
import com.phasmidsoftware.render._
import com.phasmidsoftware.util.FP._
import com.phasmidsoftware.util.Reflection

import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

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
    * @return a Table[S] where each row has value f(x) where x is the value of the corresponding row in this.
    */
  override def map[S](f: Row => S): Table[S] = unit(rows map f)

  /**
    * Transform (flatMap) this Table[Row] into a Table[S].
    *
    * @param f a function which transforms a Row into an IterableOnce[S].
    * @tparam S the type of the rows of the result.
    * @return a Table[S] which is made up of a concatenation of the results of invoking f on each row this
    */
  override def flatMap[S](f: Row => IterableOnce[S]): Table[S] = (rows map f).foldLeft(unit[S](Nil))((a, e) => a ++ unit(Iterable.from(e)))

  /**
    * Method to zip to Tables together such that the rows of the resulting table are tuples of the rows of the input tables.
    *
    * @param table the other Table.
    * @tparam R the underlying type of the other Table.
    * @return a Table of (Row, R).
    */
  def zip[R](table: Table[R]): Table[(Row, R)] = processRows[R, (Row, R)]((rs1, rs2) => rs1 zip rs2)(table)

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
  def unit[S](rows: Iterable[S]): Table[S]

  /**
    * Method to access the individual rows of this table.
    *
    * @return the rows, in the same sequence in which they were parsed.
    */
  def rows: Iterable[Row]

  /**
    * Method to yield a Seq of the rows of this Table.
    *
    * @return a Seq[Row]
    */
  override def toSeq: Seq[Row] = { lazy val rs = rows.toSeq; rs }

  /**
    * Method to yield an Array from the rows of this Table.
    *
    * @tparam Element the element type of the resulting Array.
    * @return an Array[Element].
    */
  override def toArray[Element >: Row: ClassTag]: Array[Element] = { lazy val rs = rows.toArray[Element]; rs }

  /**
    * Method to return the rows of this table as an iterator.
    *
    * @return the rows in the form of Iterator[Row]
    */
  def iterator: Iterator[Row] = rows.iterator

  /**
    * Method to process the rows as an Iterable into an Iterable which will make up the resulting Table.
    * NOTE: if you need the rows processed individually, use map or flatMap.
    *
    * @param f a function which takes an Iterable[Row] and returns an Iterable[S]
    * @tparam S the underlying type of the result.
    * @return a table[S]
    */
  def processRows[S](f: Iterable[Row] => Iterable[S]): Table[S] = unit(f(rows))

  /**
    * Method to process the rows of this Table and the other Table as a pair of Iterables resulting in an Iterable which will make up the resulting Table.
    * NOTE: this is used by zip.
    *
    * @param f     a function which takes an Iterable[Row] and an Iterable[R] and returns an Iterable[S]
    * @param other the other table of type Iterable[R].
    * @tparam R the underlying type of the other table.
    * @tparam S the underlying type of the result.
    * @return a table[S]
    */
  def processRows[R, S](f: (Iterable[Row], Iterable[R]) => Iterable[S])(other: Table[R]): Table[S] = unit(f(rows, other.rows))

  /**
    * drop
    *
    * @param n the number of rows to drop.
    * @return a Table like this Table but without its first n rows.
    */
  override def drop(n: Int): Table[Row] = processRows(rs => rs.drop(n))

  /**
    * take
    *
    * @param n the number of rows to take.
    * @return a Table like this Table but with only its first n rows.
    */
  override def take(n: Int): Table[Row] = processRows(rs => rs.take(n))

  /**
    * Method to return an empty Table of type Row.
    *
    * @return a Table[Row] without any rows.
    */
  override def empty: Table[Row] = unit(Seq.empty)

  /**
    * Method to filter the rows of a table.
    *
    * @param p a predicate to be applied to each row.
    * @return a Table[Row] consisting only of rows which satisfy the predicate p.
    */
  override def filter(p: Row => Boolean): Table[Row] = processRows(rs => rs.filter(p))

  /**
    * Method to filter out the rows of a table.
    *
    * @param p a predicate to be applied to each row.
    * @return a Table[Row] consisting only of rows which do not satisfy the predicate p.
    */
  override def filterNot(p: Row => Boolean): Table[Row] = processRows(rs => rs.filterNot(p))
}

object Table {

  /**
    * Primary method to parse a table from an Iterator of String.
    * This method is, in turn, invoked by all other parse methods defined below (other than parseSequence).
    *
    * @param ws the Strings.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](ws: Iterator[String]): Try[T] = implicitly[TableParser[T]] match {
    case parser: StringTableParser[T] => parser.parse(ws)
    case x => Failure(ParserException(s"parse method for Seq[String] incompatible with tableParser: $x"))
  }

  /**
    * Method to parse a table from an Iterable of String.
    *
    * @param ws the Strings.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](ws: Iterable[String]): Try[T] = parse(ws.iterator)

  /**
    * Method to parse a table from a Source.
    *
    * NOTE: the caller is responsible for closing the given Source.
    *
    * @param x the Source.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](x: => Source): Try[T] = for (z <- Try(x.getLines()); y <- parse(z)) yield y

  /**
    * Method to parse a table from a URI with an implicit encoding.
    *
    * @param u     the URI.
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: => URI)(implicit codec: Codec): Try[T] = safeResource(Source.fromURI(u))(parse(_))

  /**
    * Method to parse a table from a URI with an explicit encoding.
    *
    * TEST this
    *
    * @param u   the URI.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parse[T: TableParser](u: => URI, enc: String): Try[T] = {
    implicit val codec: Codec = Codec(enc)
    parse(u)
  }

  /**
    * Method to parse a table from an InputStream with an implicit encoding.
    *
    * @param i     the InputStream (call-by-name).
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseInputStream[T: TableParser](i: => InputStream)(implicit codec: Codec): Try[T] = safeResource(Source.fromInputStream(i))(parse(_))

  /**
    * Method to parse a table from an InputStream with an explicit encoding.
    *
    * TEST this
    *
    * @param i   the InputStream.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseInputStream[T: TableParser](i: => InputStream, enc: String): Try[T] = {
    implicit val codec: Codec = Codec(enc)
    parseInputStream(i)
  }

  /**
    * Method to parse a table from a File.
    *
    * TEST this.
    *
    * @param f   the File (call by name in case there is an exception thrown while constructing the file).
    * @param enc the explicit encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseFile[T: TableParser](f: => File, enc: String): Try[T] = {
    implicit val codec: Codec = Codec(enc)
    parseFile(f)
  }

  /**
    * Method to parse a table from an File.
    *
    * @param f     the File (call by name in case there is an exception thrown while constructing the file).
    * @param codec (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseFile[T: TableParser](f: => File)(implicit codec: Codec): Try[T] = safeResource(Source.fromFile(f))(parse(_))

  /**
    * Method to parse a table from a File.
    *
    * TEST this.
    *
    * @param pathname the file pathname.
    * @param enc      the explicit encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseFile[T: TableParser](pathname: String, enc: String): Try[T] = Try(parseFile(new File(pathname), enc)).flatten

  /**
    * Method to parse a table from an File.
    *
    * TEST this.
    *
    * @param pathname the file pathname.
    * @param codec    (implicit) the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseFile[T: TableParser](pathname: String)(implicit codec: Codec): Try[T] = Try(parseFile(new File(pathname))).flatten

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
    safeResource(Source.fromURL(clazz.getResource(s)))(parse(_))

  /**
    * Method to parse a table from a URL.
    *
    * @param u the URI.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseResource[T: TableParser](u: => URL)(implicit codec: Codec): Try[T] =
    for (uri <- Try(u.toURI); t <- parse(uri)) yield t

  /**
    * Method to parse a table from a URL with an explicit encoding.
    *
    * NOTE: the logic here is different from that of parseResource(u:=>URL)(implicit codec: Codec) above.
    *
    * @param u   the URL.
    * @param enc the encoding.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseResource[T: TableParser](u: => URL, enc: String): Try[T] = safeResource(Source.fromURL(u, enc))(parse(_))

  /**
    * Method to parse a table from a Seq of Seq of String.
    *
    * @param wss the Sequence of Strings.
    * @tparam T the type of the resulting table.
    * @return a Try[T]
    */
  def parseSequence[T: TableParser](wss: Iterator[Seq[String]]): Try[T] = {
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
    * TEST this.
    *
    * @param other the other Header.
    * @return a Header made up of these colukns and those of other, in that order.
    */
  def ++(other: Header): Header = Header(xs ++ other.xs)
}

object Header {

  // TODO come back and figure out why recursiveLetters (below) didn't work properly.
  lazy val numbers: LazyList[Int] = LazyList.from(1)
  lazy val generateNumbers: LazyList[String] = numbers map (_.toString)
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

  /**
    * Create a Header from a variable list of parameters.
    *
    * @param ws a variable list of Strings.
    * @return a Header.
    */
  def create(ws: String*): Header = apply(ws)
}

/**
  * CONSIDER eliminating this base class
  *
  * CONSIDER promoting render, etc. methods to Renderable.
  *
  * @param rows        the rows of the table
  * @param maybeHeader (optional) header
  * @tparam Row the underlying type of each Row
  */
abstract class BaseTable[Row](rows: Iterable[Row], val maybeHeader: Option[Header]) extends Table[Row] {
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
      case xs: Array[Any] => ww.writeRowElements(o2)(xs.toIndexedSeq)
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
    object TableRenderers extends HierarchicalRenderers {
      val rowsRenderer: Renderer[Seq[Row]] = sequenceRenderer[Row]("tbody")
      implicit val headerRenderer: Renderer[Header] = headerRenderer("tr", sequenced = false)(renderer("th", Map()))
      implicit val optionRenderer: Renderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    }
    import TableRenderers._
    val node: Node = implicitly[Renderer[Option[Header]]].render(maybeHeader)
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
    * @param rr         an (implicit) Renderer[ Indexed [ Row ] ]
    * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
    * @return a new instance of U which represents this Table as a tree of some sort.
    */
  def renderSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Indexed[Row]]): U = {
    object TableRenderers extends HierarchicalRenderers {
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
  * TEST this: it is currently not used.
  *
  * @param rows the rows of the table.
  * @tparam Row the underlying type of each Row
  */
case class UnheadedTable[Row](rows: Iterable[Row]) extends BaseTable[Row](rows, None) {
  def unit[S](rows: Iterable[S]): Table[S] = UnheadedTable(rows)
}

/**
  * Concrete case class implementing BaseTable with a Header.
  * The unit and apply methods are such that rows is in fact an Array[Row].
  *
  * NOTE: the existence or not of a Header in a BaseTable only affects how the table is rendered.
  * The parsing of a table always has a header of some sort.
  *
  * @param rows   the rows of the table, stored as an Array.
  * @param header the header.
  * @tparam Row the underlying type of each Row
  */
case class HeadedTable[Row](rows: Iterable[Row], header: Header) extends BaseTable[Row](rows, Some(header)) {
  def unit[S](rows: Iterable[S]): Table[S] = HeadedTable(rows, header)
}

/**
  * Companion object for HeadedTable.
  * The apply methods provided arbitrarily use Vector as the collection for the rows of the table.
  * CONSIDER using something else such as Array.
  */
object HeadedTable {
  def apply[Row: ClassTag](rows: Iterator[Row], header: Header): Table[Row] = HeadedTable(rows.toVector, header)

  def apply[Row: ClassTag](rows: Iterator[Row]): Table[Row] = HeadedTable(rows, Header.apply[Row]())
}

/**
  * Table Exception.
  *
  * @param w the message.
  */
case class TableException(w: String) extends Exception(w)


