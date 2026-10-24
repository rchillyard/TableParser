package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.{ColumnHelper, StringList, Strings}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.write.Writable
import java.io.{File, FileWriter}
import java.nio.file.Path
import java.time.Instant
import java.time.LocalDate
import com.phasmidsoftware.tableparser.core.util.Derivation
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.util.Try

/**
 * Type class for rendering instances to CSV.
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait CsvRenderer[-T] extends Renderer[T, String]:
  // CONSIDER removing this abstract val.
  val csvAttributes: CsvAttributes

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
object CsvRenderer extends CsvRenderers:
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
  given CsvRendererRow: CsvRenderer[Row] with
    val csvAttributes: CsvAttributes = summon[CsvAttributes]

    def render(r: Row, attrs: Map[String, String]): String = r.ws mkString csvAttributes.delimiter

  given rendererInt: CsvRenderer[Int] = CsvRenderers.CsvRendererInt
  given rendererLong: CsvRenderer[Long] = CsvRenderers.CsvRendererLong
  given rendererDouble: CsvRenderer[Double] = CsvRenderers.CsvRendererDouble
  given rendererFloat: CsvRenderer[Float] = CsvRenderers.CsvRendererFloat
  given rendererShort: CsvRenderer[Short] = CsvRenderers.CsvRendererShort
  given rendererByte: CsvRenderer[Byte] = CsvRenderers.CsvRendererByte
  given stringRenderer: CsvRenderer[String] = CsvRenderers.CsvRendererString
  given instantRenderer: CsvRenderer[Instant] = CsvRenderers.CsvRendererInstant
  given rendererBoolean: CsvRenderer[Boolean] = CsvRenderers.CsvRendererBoolean
  given rendererURL: CsvRenderer[java.net.URL] = CsvRenderers.CsvRendererURL
  given rendererStringList: CsvRenderer[StringList] = sequenceRenderer[String]
  given rendererOptionDouble: CsvRenderer[Option[Double]] = optionRenderer("no double")
  given rendererOptionInt: CsvRenderer[Option[Int]] = optionRenderer()
  given rendererOptionLong: CsvRenderer[Option[Long]] = optionRenderer[Long]()
  given rendererOptionString: CsvRenderer[Option[String]] = optionRenderer()
  given rendererOptionFloat: CsvRenderer[Option[Float]] = optionRenderer[Float]()
  given rendererOptionShort: CsvRenderer[Option[Short]] = optionRenderer[Short]()
  given rendererOptionByte: CsvRenderer[Option[Byte]] = optionRenderer[Byte]()
  given rendererOptionInstant: CsvRenderer[Option[Instant]] = optionRenderer[Instant]()
  /**
   * Method to derive a CsvRenderer for a case class T, so that `case class T(...) derives CsvRenderer` works.
   * This replaces the renderer1 ... renderer19 methods of 1.x, and has no limit on the number of fields.
   * A field whose type is itself a case class, and which has no CsvRenderer of its own, is derived in turn.
   *
   * NOTE: for a case class defined inside a method, see the NOTE on CellParser.derived.
   *
   * @param m  the Mirror of T.
   * @param ca the CsvAttributes.
   * @tparam T a case class.
   * @return a CsvRenderer[T].
   */
  inline def derived[T](using m: Mirror.ProductOf[T], ca: CsvAttributes): CsvRenderer[T] =
    new ProductCsvRenderer[T](fieldRenderers[m.MirroredElemTypes])

  private[render] inline def fieldRenderers[Ts <: Tuple](using CsvAttributes): List[CsvRenderer[?]] = inline erasedValue[Ts] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => fieldRenderer[t] :: fieldRenderers[ts]

  // Prefer a field type's own CsvRenderer; otherwise, if it is a case class, derive one.
  private inline def fieldRenderer[A](using ca: CsvAttributes): CsvRenderer[A] = summonFrom:
    case r: CsvRenderer[A] => r
    case m: Mirror.ProductOf[A] => derived[A](using m, ca)

  given localDateRenderer: CsvRenderer[LocalDate] = new CsvRenderer[LocalDate]:
    val csvAttributes: CsvAttributes = summon[CsvAttributes]

    def render(t: LocalDate, attrs: Map[String, String]): String = t.toString
//  implicit val rendererTemporal: CsvRenderer[Temporal] = new CsvRenderer[Temporal] {
//    val csvAttributes: CsvAttributes = summon[CsvAttributes]
//
//    def render(t: Temporal, attrs: Map[String, String]): String = t.toString
//  }


/**
 * Type class which combines CsvRenderer and CsvGenerator
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait CsvProduct[-T] extends CsvRenderer[T] with CsvGenerator[T]

/**
 * Companion object for CsvProduct, which renders a case class and generates its column names.
 */
object CsvProduct:
  /**
   * Method to derive a CsvProduct (both a CsvRenderer and a CsvGenerator) for a case class T,
   * so that `case class T(...) derives CsvProduct` works.
   * This replaces the rendererGenerator1 ... rendererGenerator19 methods of 1.x.
   *
   * NOTE: for a case class defined inside a method, see the NOTE on CellParser.derived.
   *
   * @param m  the Mirror of T.
   * @param ca the CsvAttributes.
   * @tparam T a case class.
   * @return a CsvProduct[T].
   */
  inline def derived[T](using m: Mirror.ProductOf[T], ca: CsvAttributes): CsvProduct[T] =
    new ProductCsvProduct[T](Derivation.fieldNames[m.MirroredElemLabels], CsvRenderer.fieldRenderers[m.MirroredElemTypes], CsvGenerator.fieldGenerators[m.MirroredElemTypes])(using ca, summonInline[ColumnHelper[T]])

/**
 * Trait `BaseCsvRenderer[-T]` which extends CsvRenderer[T]  to render, as CSV, an instance of Product (typically a case class).
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait BaseCsvRenderer[-T] extends CsvRenderer[T]:
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

/**
 * The CsvRenderer of a case class T, as derived by CsvRenderer.derived.
 * Each field is rendered by its own CsvRenderer, and the results are separated by the delimiter.
 *
 * @param fieldRenderers the CsvRenderers of the fields of T, in declaration order.
 * @param ca             the CsvAttributes.
 * @tparam T the type of the case class.
 */
class ProductCsvRenderer[T](fieldRenderers: List[CsvRenderer[?]])(using ca: CsvAttributes) extends BaseCsvRenderer[T]:
  val csvAttributes: CsvAttributes = ca

  def elements(t: T): Strings = ProductCsvRenderer.elements(t, fieldRenderers)

object ProductCsvRenderer:
  /**
   * Method to render each field of a case class with its own CsvRenderer.
   *
   * NOTE the casts: t is a case class (it has a Mirror.ProductOf), and each renderer is the one for its field.
   *
   * @param t              an instance of a case class.
   * @param fieldRenderers the CsvRenderers of the fields of t, in declaration order.
   * @return the rendered fields.
   */
  def elements(t: Any, fieldRenderers: List[CsvRenderer[?]]): Strings =
    (t.asInstanceOf[Product].productIterator zip fieldRenderers map ((x, r) => r.asInstanceOf[CsvRenderer[Any]].render(x))).toSeq

/**
 * The CsvProduct of a case class T, as derived by CsvProduct.derived: a ProductCsvGenerator which is also a CsvRenderer.
 *
 * @param fieldNames      the names of the fields of T, in declaration order.
 * @param fieldRenderers  the CsvRenderers of the fields of T, in the same order.
 * @param fieldGenerators the CsvGenerators of the fields of T, in the same order.
 * @param ca              the CsvAttributes.
 * @param helper          the ColumnHelper[T].
 * @tparam T the type of the case class.
 */
class ProductCsvProduct[T](fieldNames: List[String], fieldRenderers: List[CsvRenderer[?]], fieldGenerators: List[CsvGenerator[?]])(using ca: CsvAttributes, helper: ColumnHelper[T]) extends ProductCsvGenerator[T](fieldNames, fieldGenerators) with BaseCsvRenderer[T] with CsvProduct[T]:
  def elements(t: T): Strings = ProductCsvRenderer.elements(t, fieldRenderers)

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
abstract class CsvTableRenderer[T, O]()(using z1: CsvRenderer[T], z2: CsvGenerator[T], z3: Writable[O], ca: CsvAttributes) extends Renderer[Table[T], Try[O]]:

  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t the input parameter, i.e., the Table[T] instance to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: Table[T], attrs: Map[String, String]): Try[O] = t match
    case x: Table[?] =>
      val sw = summon[Writable[O]]
      val tc = summon[CsvRenderer[T]]
      val tg = summon[CsvGenerator[T]]
      val hdr: String = tg match
        case _tg: CsvProductGenerator[?] =>
          _tg.toColumnNames(None, None)
        case _tg: CsvGenerator[?] =>
          _tg.toColumnName(None, "")
      Try(sw.unit) map:
        o =>
          // CONSIDER can remove o2 here and just use o.
          val o2 = writeLine(sw, o, hdr)
          for (r <- x.content.toSeq) yield generateText(sw, tc, o2, r)
          o2

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
    writeLine(ow, o, tc.render(t, Map()))

  // Each line (record) ends with the newline of the CsvAttributes (by default, CRLF, as in RFC 4180).
  private def writeLine(ow: Writable[O], o: O, line: CharSequence): O =
    ow.writeRaw(ow.writeRaw(o)(line))(ca.newline)

/**
 * Case class to help render a Table to a StringBuilder in CSV format.
 *
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered; must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableStringRenderer[T]()(using z1: CsvRenderer[T], z2: CsvGenerator[T], csvAttributes: CsvAttributes) extends CsvTableRenderer[T, StringBuilder]()(using z1, z2, Writable.stringBuilderWritable(csvAttributes.delimiter, csvAttributes.quote), csvAttributes)

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * TODO merge this with CsvTableEncryptedFileRenderer to avoid duplicate code.
 *
 * @param path the path to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableFileRenderer[T](path: Path)(using z1: CsvRenderer[T], z2: CsvGenerator[T], csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(using z1, z2, Writable.fileWritable(path.toFile), csvAttributes)

/**
 * Singleton companion object providing a factory method to create instances of `CsvTableFileRenderer`.
 *
 * This object simplifies the instantiation of `CsvTableFileRenderer`, allowing the user
 * to provide a `File` directly, which is internally converted to a `Path`.
 * Implicit instances of `CsvRenderer`, `CsvGenerator`, and `CsvAttributes` are required
 * to define CSV rendering logic, header generation, and formatting options.
 */
object CsvTableFileRenderer:
  /**
   * Factory method to create an instance of CsvTableFileRenderer.
   *
   * This method constructs a CsvTableFileRenderer by converting a `File` to a `Path`
   * and utilizing the provided implicit instances of `CsvRenderer`, `CsvGenerator`,
   * and `CsvAttributes`.
   *
   * @tparam T the generic type of objects to be rendered by the CsvTableFileRenderer.
   *           Must provide evidence of the `CsvRenderer` and `CsvGenerator` type classes.
   * @param file          the `File` representing the destination to which the table will be
   *                      written as CSV.
   * @param csvAttributes an implicit instance of `CsvAttributes` defining CSV formatting
   *                      options such as delimiter and quote character.
   * @return an instance of `CsvTableFileRenderer[T]` configured for rendering tables
   *         to the specified file.
   */
  def apply[T: CsvRenderer : CsvGenerator](file: File)(using csvAttributes: CsvAttributes): CsvTableFileRenderer[T] =
    CsvTableFileRenderer(file.toPath)(using summon[CsvRenderer[T]], summon[CsvGenerator[T]], csvAttributes)
