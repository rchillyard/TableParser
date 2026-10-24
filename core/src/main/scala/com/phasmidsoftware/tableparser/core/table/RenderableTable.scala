/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.util.FieldNames
import com.phasmidsoftware.tableparser.core.write.{Node, TreeWriter, Writable}
import scala.language.postfixOps

/**
 * CONSIDER eliminating this base class or, perhaps, rename it.
 *
 * @param rows        the rows of the table
 * @param maybeHeader (optional) header
 * @tparam Row the underlying type of each Row
 */
abstract class RenderableTable[Row](rows: Content[Row], val maybeHeader: Option[Header]) extends Table[Row] with TableRenderable[Row]:
  self =>

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param sr a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  def unit[S](sr: Content[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match
    case Some(h) =>
      HeadedTable(sr, h)
    case None =>
      UnheadedTable(sr)

  /**
   *
   * @param ev implicit evidence for Renderer of Table of X.
   * @tparam O the type of the result.
   * @return an instance of O.
   */
  def render[O](using ev: Renderer[Table[Row], O]): O =
    ev.render(this)

  /**
   * CONSIDER redefining the definition of Renderer such that we can accommodate the various different types of output.
   *
   * Method to render a table in a sequential (serialized) fashion.
   *
   * @tparam O a type which supports Writable (via evidence of type Writable[O])
   * @return a new (or possibly old) instance of O.
   */
  def renderToWritable[O](using z: Writable[O]): O =
    val ww = summon[Writable[O]]
    val o1 = ww.unit
    val o2 = (maybeHeader map (h => ww.writeRaw(ww.writeRowElements(o1)(h.xs))(ww.newline))).getOrElse(o1)
    val knownSize1 = rows.knownSize
    // TODO this makes no sense now: the decision is taken inside Content.
    (if (knownSize1 > -1)
      rows.toSeq else rows.toSeq) map:
        // NOTE a Seq must be matched before a Product, which a List also is (::), else it's written as its head and tail.
        case xs: Seq[?] =>
          ww.writeRowElements(o2)(xs)
        case xs: Array[?] =>
          ww.writeRowElements(o2)(xs.toIndexedSeq)
        case p: Product =>
          ww.writeRow(o2)(p)
        case _ =>
          throw TableException("cannot render table because row is neither a Product, nor an array nor a sequence")
    o1

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
  def renderHierarchical[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(using xr: HierarchicalRenderer[Row]): U =
    object TableRenderers extends HierarchicalRenderers:
      val rowsRenderer: HierarchicalRenderer[Seq[Row]] = sequenceRenderer[Row]("tbody")
      given headerRenderer: HierarchicalRenderer[Header] = headerRenderer("tr", sequenced = false)(renderer("th", Map()))
      given optionRenderer: HierarchicalRenderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    import TableRenderers.{given, *}
    val node: Node = summon[HierarchicalRenderer[Option[Header]]].render(maybeHeader)
    summon[TreeWriter[U]].evaluate(Node(style, attributes, node +: Seq(rowsRenderer.render(rows.toSeq))))

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
  def renderHierarchicalSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(using rr: HierarchicalRenderer[Indexed[Row]]): U =
    object TableRenderers extends HierarchicalRenderers:
      val rowsRenderer: HierarchicalRenderer[Seq[Indexed[Row]]] = sequenceRenderer[Indexed[Row]]("tbody")
      given headerRenderer: HierarchicalRenderer[Header] = headerRenderer("tr", sequenced = true)(renderer("th", Map()))
      given optionRenderer: HierarchicalRenderer[Option[Header]] = optionRenderer[Header]("thead", Map())
    import TableRenderers.{given, *}
    val headerNode: Node = summon[HierarchicalRenderer[Option[Header]]].render(maybeHeader)
    val tableNode = Node(style, attributes, headerNode +: Seq(rowsRenderer.render(Indexed.index(rows.toSeq))))
    val trimmed = tableNode.trim
    summon[TreeWriter[U]].evaluate(trimmed)

  override def toString(): String =
    s"RenderableTable: header=$maybeHeader, with $size rows"

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
case class UnheadedTable[Row](content: Content[Row]) extends RenderableTable[Row](content, None):
  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param ss a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  def unit[S](ss: Iterable[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match
    case Some(h) =>
      HeadedTable(Content(ss), h)
    case None =>
      UnheadedTable(Content(ss))

  def column(name: String): Iterator[Option[String]] =
    Iterator.empty

object UnheadedTable:

  def apply[Row](rows: Iterable[Row]): Table[Row] =
    new UnheadedTable(Content(rows))

  def apply[Row](rows: Iterator[Row]): Table[Row] =
    UnheadedTable(rows.to(List))


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
case class HeadedTable[Row](content: Content[Row], header: Header) extends RenderableTable[Row](content, Some(header)):

  /**
   * Method to generate a Table[S] for a set of rows.
   * Although declared as an instance method, this method produces its result independent of this.
   *
   * @param ss a sequence of S.
   * @tparam S the underlying type of the rows and the result.
   * @return a new instance of Table[S].
   */
  def unit[S](ss: Iterable[S], maybeHeader: Option[Header]): Table[S] = maybeHeader match
    case Some(h) =>
      HeadedTable(Content(ss), h)
    case None =>
      UnheadedTable(Content(ss))

  /**
   * Method to get all the elements of a particular column.
   *
   * Complexity: N + C where N is the number of rows and C is the number of columns.
   *
   * @param name the column name.
   * @return an Iterator of Option[String].
   */
  def column(name: String): Iterator[Option[String]] =
    val maybeIndex = maybeColumnNames map (_.indexOf(name))
    content.iterator map:
      case row: RawRow =>
        maybeIndex map (row.ws(_))
      case ws: Seq[Any] =>
        maybeIndex map (ws(_).toString)
      case _ =>
        None

  override def toString(): String =
    s"HeadedTable($header) with ${content.size} rows"

/**
 * Companion object for HeadedTable.
 * The apply methods provided arbitrarily use Vector as the collection for the rows of the table.
 * CONSIDER using something else such as Array.
 *
 * TESTME ?
 */
object HeadedTable:

  def apply[Row](rows: Iterable[Row], header: Header): RenderableTable[Row] =
    new HeadedTable(Content(rows), header)

  def apply[Row: FieldNames](rows: Iterable[Row]): RenderableTable[Row] =
    HeadedTable(rows, Header[Row]())

  def apply[Row](rows: Iterator[Row], header: Header): RenderableTable[Row] =
    HeadedTable(rows.to(List), header)

  def apply[Row: FieldNames](rows: Iterator[Row]): RenderableTable[Row] =
    HeadedTable(rows, Header[Row]())

