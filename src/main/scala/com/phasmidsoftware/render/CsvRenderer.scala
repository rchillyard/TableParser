package com.phasmidsoftware.render

import cats.effect.IO
import com.phasmidsoftware.crypto.HexEncryption
import com.phasmidsoftware.parse.Strings
import com.phasmidsoftware.table._
import com.phasmidsoftware.write.Writable
import java.io.{File, FileWriter}
import scala.reflect.ClassTag

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
 * Type class which combines CsvRenderer and CsvGenerator
 *
 * @tparam T the contravariant type of object to be rendered.
 */
trait CsvProduct[-T] extends CsvRenderer[T] with CsvGenerator[T]

/**
 * Abstract class ProductCsvRenderer which extends CsvRenderer AND CsvGenerator to render, as CSV, an instance of Product (typically a case class).
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
   * @param t     the input parameter, i.e. the T object to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type String.
   */
  def render(t: T, attrs: Map[String, String]): String = elements(t) mkString csvAttributes.delimiter
}

/**
 * Abstract class ProductCsvRenderer which extends CsvRenderer AND CsvGenerator to render, as CSV, an instance of Product (typically a case class).
 *
 * @param c (implicit) CsvAttributes
 * @tparam T the contravariant type of object to be rendered.
 */
abstract class ProductCsvRenderer[T <: Product : ClassTag](implicit c: CsvAttributes) extends BaseCsvProductGenerator[T] with BaseCsvRenderer[T] with CsvProduct[T]

abstract class CsvTableRenderer[T: CsvRenderer : CsvGenerator : Ordering, O: Writable] extends Renderer[Table[T], IO[O]] {

  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t     the input parameter, i.e. the Table[T] instance to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: Table[T], attrs: Map[String, String]): IO[O] = t match {
    case x: Table[_] =>
      val sw = implicitly[Writable[O]]
      val tc = implicitly[CsvRenderer[T]]
      val tg = implicitly[CsvGenerator[T]]
      val hdr: String = tg match {
        case _tg: CsvProductGenerator[_] => _tg.toColumnNames(None, None)
        case _tg: CsvGenerator[_] => _tg.toColumnName(None, "")
      }
      IO(sw.unit) map {
        o =>
          // CONSIDER can remove o2 here and just use o.
          val o2 = sw.writeRawLine(o)(hdr)
          for (r <- x.content.ordered) yield generateText(sw, tc, o2, r)
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
  protected def generateText(ow: Writable[O], tc: CsvRenderer[T], o: O, t: T): O = ow.writeRawLine(o)(tc.render(t, Map()))
}

/**
 * Case class to help render a Table to a StringBuilder in CSV format.
 *
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableStringRenderer[T: CsvRenderer : CsvGenerator : Ordering]()(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, StringBuilder]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], implicitly[Ordering[T]], Writable.stringBuilderWritable(csvAttributes.delimiter, csvAttributes.quote))

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * TODO merge this with CsvTableEncryptedFileRenderer to avoid duplicate code.
 *
 * @param file          the file to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 */
case class CsvTableFileRenderer[T: CsvRenderer : CsvGenerator : Ordering](file: File)(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], implicitly[Ordering[T]], Writable.fileWritable(file))

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * TODO remove duplicate code
 *
 * TESTME
 *
 * @param file          the file to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
 */
case class CsvTableEncryptedFileRenderer[T: CsvRenderer : CsvGenerator : Ordering : HasKey, A: HexEncryption](file: File)(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], implicitly[Ordering[T]], Writable.fileWritable(file)) {
  override protected def generateText(ow: Writable[FileWriter], tc: CsvRenderer[T], o: FileWriter, t: T): FileWriter = {
    val key = implicitly[HasKey[T]].key(t)
    val rendering = tc.render(t, Map())
    ow.writeLineEncrypted(o)(key, rendering)
  }
}
