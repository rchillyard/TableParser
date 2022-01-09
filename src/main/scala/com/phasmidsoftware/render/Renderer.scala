/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table._
import java.io.{File, FileWriter}
import org.joda.time.LocalDate
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/**
 * Definition of trait Renderer for the purpose of serializing objects of type T as an object of type O.
 * This trait may be used as a type class for either T or O (or both).
 *
 * NOTE: this trait has no direct relationship with Renderable.
 *
 * @tparam T the type of object to be rendered.
 * @tparam O the type of the serialization result.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of Renderer[${T},{O}].")
trait Renderer[T, O] {

  /**
   * Render an instance of T as an O, qualifying the rendering with no attributes.
   *
   * @param t a T to be rendered.
   * @return an instance of type O.
   */
  def render(t: T): O = render(t, Map())

  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t     the input parameter, i.e. the T object to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: T, attrs: Map[String, String]): O
}

/**
 * Definition of type class HierarchicalRenderer for the purpose of serializing objects of type T.
 * Since, in general, T will be a case class which may include parameters which are case classes,
 * we render to a hierarchical type: Node.
 *
 * @tparam T the type of object to be rendered.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of HierarchicalRenderer[${T}].")
trait HierarchicalRenderer[T] extends Renderer[T, Node] {

  /**
   * Defines the default style for type T.
   */
  val style: String

  /**
   * Defines the base attribute set for type T.
   */
  val baseAttrs: Map[String, String] = Map()

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node

  /**
   * Method to render content as a String.
   * This method is invoked only when T is not a Product, sequence or Option.
   * Normally, the default method is what is required, but it might be necessary to override
   * in some situations.
   * This method does not apply to style or attribute values.
   *
   * @param t the content value.
   * @return a String corresponding to t.
   */
  def asString(t: T): String = t.toString
}

/**
 * CONSIDER having style defined as an Option[String]
 *
 * @tparam T the type of object to be rendered.
 */
trait UntaggedHierarchicalRenderer[T] extends HierarchicalRenderer[T] {
  val style: String = ""

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)

}

abstract class TaggedHierarchicalRenderer[T](val style: String, override val baseAttrs: Map[String, String] = Map()) extends HierarchicalRenderer[T] {

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)
}

abstract class ProductHierarchicalRenderer[T <: Product : ClassTag](val style: String, override val baseAttrs: Map[String, String] = Map()) extends HierarchicalRenderer[T] {
  def render(t: T, attrs: Map[String, String]): Node = Node(style, attrs, nodes(t))

  protected def nodes(t: T): Seq[Node]
}

object HierarchicalRenderer {

  trait StringHierarchicalRenderer extends UntaggedHierarchicalRenderer[String]

  implicit object StringHierarchicalRenderer extends StringHierarchicalRenderer

  trait BooleanHierarchicalRenderer extends UntaggedHierarchicalRenderer[Boolean]

  implicit object BooleanHierarchicalRenderer extends BooleanHierarchicalRenderer

  trait IntHierarchicalRenderer extends UntaggedHierarchicalRenderer[Int]

  implicit object IntHierarchicalRenderer extends IntHierarchicalRenderer

  trait LongHierarchicalRenderer extends UntaggedHierarchicalRenderer[Long]

  implicit object LongHierarchicalRenderer extends LongHierarchicalRenderer

  trait BigIntHierarchicalRenderer extends UntaggedHierarchicalRenderer[BigInt]

  implicit object BigIntHierarchicalRenderer extends BigIntHierarchicalRenderer

  trait DoubleHierarchicalRenderer extends UntaggedHierarchicalRenderer[Double]

  implicit object DoubleHierarchicalRenderer extends DoubleHierarchicalRenderer

  trait LocalDateHierarchicalRenderer extends UntaggedHierarchicalRenderer[LocalDate]

  implicit object LocalDateHierarchicalRenderer extends LocalDateHierarchicalRenderer
}

/**
 * Type class for rendering instances to CSV.
 *
 * @tparam T the type of object to be rendered.
 */
trait CsvRenderer[T] extends Renderer[T, String] {
  // CONSIDER removing this abstract val.
  val csvAttributes: CsvAttributes
}

abstract class CsvTableRenderer[T: CsvRenderer : CsvGenerator, O: Writable]()(implicit csvAttributes: CsvAttributes) extends Renderer[Table[T], O] {
  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t     the input parameter, i.e. the Table[T] instance to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: Table[T], attrs: Map[String, String]): O = t match {
    case x: Table[_] =>
      val sw = implicitly[Writable[O]]
      val tc = implicitly[CsvRenderer[T]]
      val tg = implicitly[CsvGenerator[T]]
      val hdr: String = tg match {
        case _tg: CsvProductGenerator[_] => _tg.toColumnNames(None, None)
        case _tg: CsvGenerator[_] => _tg.toColumnName(None, "")
      }
      val o = sw.unit
      sw.writeRawLine(o)(hdr)
      for (x <- x.rows.toSeq) generateText(sw, tc, o, x)
      sw.close(o)
      o
  }

  /**
   * CONSIDER replacing ow by implicitly of Writable[O].
   * CONSIDER replacing tc by implicitly of CsvRenderer[T].
   *
   * @param ow Writable[O].
   * @param tc CsvRenderer[T].
   * @param o  O.
   * @param t  T.
   * @return O.
   */
  protected def generateText(ow: Writable[O], tc: CsvRenderer[T], o: O, t: T): O = ow.writeRawLine(o)(tc.render(t, Map()))
}


/**
 * Case class to help render a Table to a StringBuilder in CSV format.
 *
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableStringRenderer[T: CsvRenderer : CsvGenerator]()(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, StringBuilder]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.stringBuilderWritable(csvAttributes.delimiter, csvAttributes.quote), csvAttributes)

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * @param file          the file to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableFileRenderer[T: CsvRenderer : CsvGenerator](file: File)(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.fileWritable(file), csvAttributes)

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * @param file          the file to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableEncryptedFileRenderer[T: CsvRenderer : CsvGenerator : HasKey](file: File)(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.fileWritable(file), csvAttributes) {
  override protected def generateText(ow: Writable[FileWriter], tc: CsvRenderer[T], o: FileWriter, t: T): FileWriter = {
    val key = implicitly[HasKey[T]].key(t)
    val rendering = tc.render(t, Map())
    import com.phasmidsoftware.crypto.EncryptionAES128CTR
    implicit val encryption: EncryptionAES128CTR.type = EncryptionAES128CTR
    ow.writeLineEncrypted(o)(key, rendering)
  }
}


