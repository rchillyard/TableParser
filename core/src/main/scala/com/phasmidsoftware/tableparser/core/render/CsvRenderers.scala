/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.Strings
import com.phasmidsoftware.tableparser.core.table._
import java.net.URL
import java.time.Instant

/**
 * Trait to define various renderers for rendering instance of case classes (with their various parameters),
 * containers (Seq and Option), etc. to CSV output.
 *
 * CONSIDER a mechanism to ensure that objects involving case classes are presented in the same order as specified by the header.
 */
trait CsvRenderers {

  /**
   * Method to return a CsvRenderer[RawRow].
   *
   * @param ca the (implicit) CsvAttributes.
   * @return a CsvRenderer[RawRow].
   */
  def rawRowRenderer(using ca: CsvAttributes): CsvRenderer[RawRow] = new CsvRenderer[RawRow] {
    given z: CsvRenderer[String] = CsvRenderers.CsvRendererString

    /**
     * Renders a sequence of strings based on the provided `RawRow` and attribute map.
     *
     * @param t     The `RawRow` instance containing the sequence of strings to be rendered.
     * @param attrs A map of attributes that can be used to influence the rendering process.
     *              Keys are attribute names, and values are their corresponding values.
     * @return A `String` representation of the rendered sequence.
     */
    def render(t: RawRow, attrs: Map[String, String]): String =
      sequenceRenderer[String].render(t.ws)

    val csvAttributes: CsvAttributes = ca
  }

  /**
   * Method to return a CsvRenderer[ Seq[T] ].
   *
   * @param ca the (implicit) CsvAttributes.
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[ Seq[T] ]
   */
  def sequenceRenderer[T: CsvRenderer](using ca: CsvAttributes): CsvRenderer[Seq[T]] = new CsvRenderer[Seq[T]] {

    /**
     * Renders a sequence of objects into a CSV-formatted string using provided attributes.
     *
     * @param ts    The sequence of objects of type `T` to be rendered into CSV format.
     * @param attrs A map of attribute key-value pairs representing options or settings for the CSV rendering process.
     * @return A string representing the sequence `ts` in CSV format, concatenated using the delimiter defined in `csvAttributes`.
     */
    def render(ts: Seq[T], attrs: Map[String, String]): String =
      (ts map { (t: T) => summon[CsvRenderer[T]].render(t) }).mkString(csvAttributes.delimiter)

    val csvAttributes: CsvAttributes = ca
  }

  /**
   * Method to return a CsvRenderer[ Option[T] ].
   *
   * @param ca the (implicit) CsvAttributes.
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[ Option[T] ].
   */
  def optionRenderer[T: CsvRenderer](defaultString: String = "")(using ca: CsvAttributes): CsvRenderer[Option[T]] = new CsvRenderer[Option[T]] {
    val csvAttributes: CsvAttributes = ca

    /**
     * Renders an optional value of type `T` into a string representation using a `CsvRenderer`
     * and applies default string rendering if the value is absent.
     *
     * @param to    An optional value of type `T` that needs to be rendered. If present, it will
     *              be rendered using the implicitly provided `CsvRenderer[T]`.
     * @param attrs A map of string attributes that can optionally be used during
     *              the rendering process. Currently, these are not utilized in the logic.
     * @return A string representation of the provided value if present, rendered using
     *         the `CsvRenderer[T]`. Otherwise, returns a default string.
     */
    def render(to: Option[T], attrs: Map[String, String]): String =
      (to map (t => summon[CsvRenderer[T]].render(t))).getOrElse(defaultString)
  }

  /**
   * Method to return a CsvRenderer[Option[T]] for a T which has several columns (e.g. a case class).
   * None is rendered as the right number of empty cells, so that the columns which follow stay aligned.
   * Use this together with CsvGenerators.optionProductGenerator.
   * (optionRenderer renders None as just one cell.)
   *
   * @param r  the CsvRenderer[T].
   * @param g  the CsvGenerator[T], which determines how many columns a T has.
   * @param ca the CsvAttributes.
   * @tparam T the underlying type.
   * @return a CsvRenderer[Option[T]].
   */
  def optionProductRenderer[T](using r: CsvRenderer[T], g: CsvGenerator[T], ca: CsvAttributes): CsvRenderer[Option[T]] = new CsvRenderer[Option[T]] {
    val csvAttributes: CsvAttributes = ca

    // The number of columns of a T.
    private lazy val columns: Int = g.toColumnName(None, "x").split(java.util.regex.Pattern.quote(ca.delimiter)).length

    def render(to: Option[T], attrs: Map[String, String]): String = to match {
      case Some(t) => r.render(t, attrs)
      case None => ca.delimiter * (columns - 1)
    }
  }

  /**
   * Method to return a CsvRenderer[T] which does not output a T at all, only a number of delimiters according to the value of alignment.
   *
   * @param alignment (defaults to 1): one more than the number of delimiters to output.
   *                  If you are skipping a Product (such as a case class instance), then you should carefully count up how many (nested) elements to skip.
   *                  So, for example, if you are skipping a Product with three members, you would set alignment = 3, even though you only want to output 2 delimiters.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam T the type of the parameter to the render method.
   * @return a CsvRenderer[T].
   */
  def skipRenderer[T](alignment: Int = 1)(using ca: CsvAttributes): CsvRenderer[T] = new CsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    /**
     * Renders the given input along with specified attributes into a formatted string.
     *
     * @param t     The input of type `T` to be rendered.
     * @param attrs A map of attribute key-value pairs that provide additional rendering parameters.
     * @return A formatted string representation of the input `t` based on the attributes and internal logic.
     */
    def render(t: T, attrs: Map[String, String]): String =
      ca.delimiter * (alignment - 1)
  }

// ── 14 ──────────────────────────────────────────────────────────────────────

  // ── 15 ──────────────────────────────────────────────────────────────────────

  // ── 16 ──────────────────────────────────────────────────────────────────────

  // ── 17 ──────────────────────────────────────────────────────────────────────

  // ── 18 ──────────────────────────────────────────────────────────────────────

  // ── 19 ──────────────────────────────────────────────────────────────────────

}

/**
 * The `CsvRenderers` object provides a collection of implicit `StandardCsvRenderer` implementations
 * for common data types. These renderers are used to convert data of specific types into CSV
 * (Comma-Separated Values) format according to the defined rendering logic.
 */
object CsvRenderers {
  /**
   * The `StandardCsvRenderer` is an abstract class that provides a base implementation of the
   * `CsvRenderer` trait for rendering objects of type `T` into CSV (Comma-Separated Values) format.
   *
   * This class is typically extended to create concrete renderers for specific types. By default,
   * it implements the `render` method to simply convert the input object to its string representation,
   * but this behavior may be overridden in subclasses to implement more specific serialization logic.
   *
   * @tparam T The type of object to be rendered into CSV format.
   */
  abstract class StandardCsvRenderer[T] extends CsvRenderer[T] {
    val csvAttributes: CsvAttributes = summon[CsvAttributes]

    /**
     * Renders an object of type `T` into its string representation.
     * Additional attributes can be specified via a map, but they are not used in the default implementation.
     *
     * @param t     the object of type `T` to be rendered.
     * @param attrs a map of attributes influencing the rendering process; not used in the default implementation.
     * @return a string representation of the object `t`.
     */
    def render(t: T, attrs: Map[String, String]): String = t.toString
  }

  /**
   * An implicit object for rendering `Boolean` values into CSV (Comma-Separated Values) format.
   *
   * `CsvRendererBoolean` extends the `StandardCsvRenderer` abstraction, which provides
   * a base implementation for converting objects into their CSV string representation.
   *
   * This implicit object handles the rendering of `Boolean` values by converting them
   * to their string representation, typically as `"true"` or `"false"`.
   *
   * Usage:
   * The `CsvRendererBoolean` can be implicitly used wherever a `StandardCsvRenderer[Boolean]`
   * is required, enabling seamless conversion of `Boolean` values to CSV-compatible strings.
   *
   * Example:
   * {{{
   * // Assuming an implicit `CsvAttributes` in scope
   * val renderer = summon[StandardCsvRenderer[Boolean]]
   * val csvValue = renderer.render(true, Map.empty)
   * println(csvValue) // Output: "true"
   * }}}
   */
  implicit object CsvRendererBoolean extends StandardCsvRenderer[Boolean]

  /**
   * Implicit object `CsvRendererInt` provides a concrete implementation of the `StandardCsvRenderer`
   * class for rendering `Int` values into CSV (Comma-Separated Values) format.
   *
   * This object allows the rendering of integer values into their string representation for use
   * in CSV output while adhering to the conventions defined in the `StandardCsvRenderer` abstraction.
   *
   * Being defined as implicit, it can be automatically utilized in any context where a
   * `StandardCsvRenderer[Int]` is required, enabling seamless integration into systems that
   * rely on type class-based serialization for CSV export.
   */
  implicit object CsvRendererInt extends StandardCsvRenderer[Int]

  /**
   * Implicit object `CsvRendererLong` provides a `StandardCsvRenderer` implementation specifically for the `Long` type.
   *
   * This renderer is used to convert `Long` values into their CSV (Comma-Separated Values) string representation.
   * It extends the `StandardCsvRenderer` abstract class, which defines a default rendering mechanism by
   * converting the input object to its `String` representation.
   *
   * As an implicit object, `CsvRendererLong` can be automatically applied wherever a `StandardCsvRenderer[Long]`
   * is required in the context of CSV rendering operations, such as serializing data of type `Long` into CSV format.
   *
   * Example usage:
   * {{{
   * implicit val csvAttributes: CsvAttributes = CsvAttributes() // Example of CSV configuration
   * val longValue: Long = 123456789L
   * val csvString: String = CsvRendererLong.render(longValue, Map.empty) // "123456789"
   * }}}
   *
   * Note: Additional attributes passed in the `Map` parameter to the `render` method are supported but not
   * utilized in this implementation.
   */
  implicit object CsvRendererLong extends StandardCsvRenderer[Long]

  /**
   * `CsvRendererDouble` is an implicit object that provides a mechanism to render instances
   * of type `Double` into a CSV (Comma-Separated Values) format. It extends the
   * `StandardCsvRenderer` abstract class, inheriting its generic rendering capabilities,
   * and specializes the functionality for `Double` values.
   *
   * This implicit object allows seamless and type-safe rendering of `Double`s into strings
   * formatted for CSV output by leveraging Scala's implicit resolution mechanism.
   *
   * Example usage:
   * {{{
   * import CsvRenderers._
   *
   * val value: Double = 123.456
   * val renderedValue: String = CsvRendererDouble.render(value)
   *
   * println(renderedValue) // Output: "123.456"
   * }}}
   *
   * The `CsvRendererDouble` relies on the default implementation of `render` in the
   * `StandardCsvRenderer` class, where the `Double` is converted to its string
   * representation. Custom attributes influencing the rendering may be passed via a map,
   * though they are unused in the default implementation.
   *
   * @see [[com.phasmidsoftware.tableparser.core.render.StandardCsvRenderer]]
   * @see [[com.phasmidsoftware.tableparser.core.render.Renderer]]
   */
  implicit object CsvRendererDouble extends StandardCsvRenderer[Double]

  /**
   * Implicit object that provides a CSV renderer for `Float` values by extending the `StandardCsvRenderer`
   * abstract class with type parameter `Float`.
   *
   * This renderer converts `Float` values into their CSV string representation, utilizing the default
   * behavior from the `StandardCsvRenderer` class unless overridden. The implicit nature of this object
   * allows it to be automatically used wherever a `StandardCsvRenderer` for `Float` is required.
   *
   * This renderer adheres to the base `CsvRenderer` trait interface, enabling consistent rendering
   * of `Float` values in CSV-related operations.
   */
  implicit object CsvRendererFloat extends StandardCsvRenderer[Float]

  /**
   * Implicit object providing a CSV renderer for the `Short` type.
   *
   * This object extends the `StandardCsvRenderer` class, implementing
   * the base functionality to render `Short` values into their string
   * representation in CSV format. It can be used implicitly wherever
   * a `CsvRenderer[Short]` is required.
   *
   * It adheres to the default behavior of `StandardCsvRenderer`, which
   * converts the given value to its string representation without
   * applying additional attributes or custom processing.
   */
  implicit object CsvRendererShort extends StandardCsvRenderer[Short]

  /**
   * An implicit implementation of the `StandardCsvRenderer` for the `Byte` type.
   *
   * The `CsvRendererByte` object provides functionality to render `Byte` values into CSV format
   * by extending the `StandardCsvRenderer` abstract class. This implementation defines the specific
   * rendering behavior for converting `Byte` type data into its CSV-compatible string representation.
   */
  implicit object CsvRendererByte extends StandardCsvRenderer[Byte]

  /**
   * `CsvRendererString` is an implicit object that extends the `StandardCsvRenderer` class
   * to provide a concrete implementation of CSV rendering for `String` objects.
   *
   * This class allows a `String` to be rendered into its CSV representation. By default,
   * it inherits the behavior of the `StandardCsvRenderer`, which typically renders the input
   * by converting it to a string representation (for strings, this is simply the value itself).
   *
   * Example Usage:
   * {{{
   *   val csvString: String = CsvRendererString.render("example")
   * }}}
   */
  /**
   * Method to quote a String as a CSV field (as in RFC 4180), if it has to be quoted to be read back as it is:
   * that is, if it contains the delimiter, the quote character or a line break, or if it starts or ends with whitespace.
   * Any quote characters inside it are doubled.
   *
   * @param w  the String.
   * @param ca the CsvAttributes (for the delimiter and the quote character).
   * @return the String, quoted if necessary.
   */
  def quoteIfNecessary(w: String)(using ca: CsvAttributes): String = {
    val quote = ca.quote
    val delimiter = ca.delimiter.trim match {
      case "" => ca.delimiter
      case d => d
    }
    val mustQuote = quote.nonEmpty && w.nonEmpty &&
            (w.contains(delimiter) || w.contains(quote) || w.exists(c => c == '\n' || c == '\r') || w.head.isWhitespace || w.last.isWhitespace)
    if (mustQuote) quote + w.replace(quote, quote + quote) + quote else w
  }

  implicit object CsvRendererString extends StandardCsvRenderer[String] {
    /**
     * Method to render a String as a CSV cell, quoting it (as in RFC 4180) when it has to be quoted to be read back as it is:
     * when it contains the delimiter, the quote character or a line break, or when it starts or ends with whitespace
     * (which would otherwise be taken as part of the delimiter). Any quote characters inside it are doubled.
     *
     * @param w     the String.
     * @param attrs (unused) attributes.
     * @return the String, quoted if necessary.
     */
    override def render(w: String, attrs: Map[String, String]): String =
      CsvRenderers.quoteIfNecessary(w)(using csvAttributes)
  }

  /**
   * Provides an implicit instance of `StandardCsvRenderer` for the type `java.net.URL`.
   *
   * This object is used to render `URL` instances into their CSV (Comma-Separated Values) string representation.
   * It extends the `StandardCsvRenderer` class, leveraging its default implementation for converting objects
   * into a string format. Specifically, for `URL` instances, the `toString` method of the URL is used as the
   * rendered string by default, unless overridden.
   *
   * The renderer is intended for scenarios where `URL` objects need to be serialized or exported in CSV format,
   * commonly for data processing or storage purposes.
   *
   * Example usage:
   * {{{
   * val url: URL = new URL("https://example.com")
   * val renderedCsv: String = summon[StandardCsvRenderer[URL]].render(url, Map())
   * // renderedCsv would contain "https://example.com"
   * }}}
   *
   * This implicit object allows seamless usage of `URL` type rendering in contexts where `StandardCsvRenderer`
   * is required, such as within collections or for pipeline-based CSV serialization.
   */
  implicit object CsvRendererURL extends StandardCsvRenderer[URL]

  implicit object CsvRendererInstant extends CsvRenderer[Instant] {
    val csvAttributes: CsvAttributes = summon[CsvAttributes]

    def render(t: Instant, attrs: Map[String, String]): String = t.toString
  }

//  implicit object CsvRendererTemporal extends CsvRenderer[Temporal] {
//    val csvAttributes: CsvAttributes = summon[CsvAttributes]
//    def render(t: Temporal, attrs: Map[String, String]): String = t.toString
//  }
}
