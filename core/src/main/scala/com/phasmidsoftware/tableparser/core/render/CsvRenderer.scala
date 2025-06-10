package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.{StringList, Strings}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.write.Writable
import java.io.{File, FileWriter}
import org.joda.time.LocalDate
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Type class for rendering instances to CSV.
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait CsvRenderer[-T] extends Renderer[T, String] {
  // CONSIDER removing this abstract val.
  val csvAttributes: CsvAttributes
}

/**
 * An object that provides type class support for rendering data as CSV (Comma-Separated Values).
 *
 * The `CsvRenderer` object is designed to define implicit instances of `CsvRenderer` for specific types,
 * allowing those types to be rendered to a CSV format string. It relies on the `Renderer` type class,
 * which generalizes rendering objects of type `T` into objects of type `O` (in this case, strings).
 *
 * This object can include predefined implicit renderers (e.g., `Row` renderer) that leverage attributes
 * (`Map[String, String]`) to modify the output or handle special behaviors for CSV attributes.
 */
object CsvRenderer extends CsvRenderers {
  /**
   * Implicit object that provides a CSV rendering implementation for the `Row` class.
   *
   * This object extends the `CsvRenderer` type class for `Row`, enabling rows to be
   * rendered as CSV strings. The default behavior concatenates the row elements (`ws`)
   * into a single string, separated by the delimiter defined in the implicit `CsvAttributes`.
   *
   * @see CsvRenderer
   * @see Row
   */
  implicit object CsvRendererRow extends CsvRenderer[Row] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(r: Row, attrs: Map[String, String]): String = r.ws mkString csvAttributes.delimiter
  }

  implicit val rendererInt: CsvRenderer[Int] = CsvRenderers.CsvRendererInt
  implicit val rendererDouble: CsvRenderer[Double] = CsvRenderers.CsvRendererDouble
  implicit val stringRenderer: CsvRenderer[String] = CsvRenderers.CsvRendererString
  implicit val rendererStringList: CsvRenderer[StringList] = sequenceRenderer[String]
  implicit val rendererOptionDouble: CsvRenderer[Option[Double]] = optionRenderer("no double")
  implicit val rendererOptionInt: CsvRenderer[Option[Int]] = optionRenderer()
  implicit val rendererOptionString: CsvRenderer[Option[String]] = optionRenderer()

  implicit val localDateRenderer: CsvRenderer[LocalDate] = new CsvRenderer[LocalDate] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(t: LocalDate, attrs: Map[String, String]): String = t.toString
  }

}

/**
 * Type class which combines CsvRenderer and CsvGenerator
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait CsvProduct[-T] extends CsvRenderer[T] with CsvGenerator[T]

/**
 * Trait `BaseCsvRenderer[-T]` which extends CsvRenderer[T]  to render, as CSV, an instance of Product (typically a case class).
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait BaseCsvRenderer[-T] extends CsvRenderer[T] {
  /**
   * Method to obtain a Seq of Strings corresponding to each of the members of the given value of t.
   *
   * @param t an instance of type T.
   * @return a Strings, i.e. a Seq[String].
   */
  def elements(t: T): Strings

  /**
   * Concrete method to render t as a single String, where the columns are delimited according to <code>csvAttributes.delimiter</code>.
   *
   * @param t the input parameter, i.e., the T object to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type String.
   */
  def render(t: T, attrs: Map[String, String]): String =
    elements(t) mkString csvAttributes.delimiter
}

/**
 * Abstract class `ProductCsvRenderer[T]` which extends `BaseCsvRenderer[T]` and `CsvGenerator[T]` and `CsvProduct[T]` to render, as CSV, an instance of Product (typically a case class).
 *
 * @param c (implicit) CsvAttributes
 * @tparam T the contravariant type of object to be rendered.
 */
abstract class ProductCsvRenderer[T <: Product : ClassTag](implicit c: CsvAttributes) extends BaseCsvProductGenerator[T] with BaseCsvRenderer[T] with CsvProduct[T]

/**
 * Abstract class for rendering a tabular representation of type `Table[T]` to a sequentially writable format `O`,
 * such as CSV, while utilizing implicit type classes for rendering cells and generating column names.
 *
 * This class simplifies the conversion of tabular data (`Table[T]`) into a structured format (`O`) with support for customization
 * through attributes and allows seamless integration with CSV-specific typeclasses.
 *
 * @tparam T the type of elements contained within the table; must have implicit evidence for `CsvRenderer` and `CsvGenerator`.
 * @tparam O the type of the writable output; must have implicit evidence for `Writable`.
 */
abstract class CsvTableRenderer[T: CsvRenderer : CsvGenerator, O: Writable] extends Renderer[Table[T], Try[O]] {

  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t the input parameter, i.e., the Table[T] instance to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: Table[T], attrs: Map[String, String]): Try[O] = t match {
    case x: Table[_] =>
      val sw = implicitly[Writable[O]]
      val tc = implicitly[CsvRenderer[T]]
      val tg = implicitly[CsvGenerator[T]]
      val hdr: String = tg match {
        case _tg: CsvProductGenerator[_] =>
          _tg.toColumnNames(None, None)
        case _tg: CsvGenerator[_] =>
          _tg.toColumnName(None, "")
      }
      Try(sw.unit) map {
        o =>
          // CONSIDER can remove o2 here and just use o.
          val o2 = sw.writeRawLine(o)(hdr)
          for (r <- x.content.toSeq) yield generateText(sw, tc, o2, r)
          o2
      }
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
  protected def generateText(ow: Writable[O], tc: CsvRenderer[T], o: O, t: T): O =
    ow.writeRawLine(o)(tc.render(t, Map()))
}

/**
 * Case class to help render a Table to a StringBuilder in CSV format.
 *
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered; must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableStringRenderer[T]()(implicit z1: CsvRenderer[T], z2: CsvGenerator[T], csvAttributes: CsvAttributes) extends CsvTableRenderer[T, StringBuilder]()(z1, z2, Writable.stringBuilderWritable(csvAttributes.delimiter, csvAttributes.quote))

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * TODO merge this with CsvTableEncryptedFileRenderer to avoid duplicate code.
 *
 * @param file          the file to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableFileRenderer[T: CsvRenderer : CsvGenerator](file: File)(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.fileWritable(file))
