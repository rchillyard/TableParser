package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import java.net.URL
import java.time.Instant
import org.joda.time.LocalDate

/**
 * The `CsvGenerators` class provides factory methods for creating `CsvGenerator` instances for different types of data structures.
 * These generators can be used to serialize data into CSV (Comma-Separated Values) format.
 *
 * The class includes methods to handle sequences, options, individual types, and products (case classes with varying numbers of fields).
 * It supports up to 6-ary product types, allowing for type inference and customization of CSV behavior.
 */
trait CsvGenerators {

  /**
   * Method to return a CsvGenerator[ Seq[T] ].
   *
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[ Seq[T] ]
   */
  def sequenceGenerator[T](using ca: CsvAttributes): CsvGenerator[Seq[T]] = new StandardCsvGenerator[Seq[T]]

  /**
   * Method to return a CsvGenerator[ Option[T] ].
   *
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[ Option[T] ].
   */
  def optionGenerator[T](using ca: CsvAttributes): CsvGenerator[Option[T]] = new StandardCsvGenerator[Option[T]]

  implicit val optionIntGenerator: CsvGenerator[Option[Int]] = optionGenerator
  implicit val optionDoubleGenerator: CsvGenerator[Option[Double]] = optionGenerator
  implicit val optionStringGenerator: CsvGenerator[Option[String]] = optionGenerator
  implicit val optionBooleanGenerator: CsvGenerator[Option[Boolean]] = optionGenerator
  implicit val optionLocalDateGenerator: CsvGenerator[Option[LocalDate]] = optionGenerator
  implicit val optionLongGenerator: CsvGenerator[Option[Long]] = optionGenerator
  implicit val optionBigIntGenerator: CsvGenerator[Option[BigInt]] = optionGenerator
  implicit val optionFloatGenerator: CsvGenerator[Option[Float]] = optionGenerator
  implicit val optionShortGenerator: CsvGenerator[Option[Short]] = optionGenerator
  implicit val optionByteGenerator: CsvGenerator[Option[Byte]] = optionGenerator
  implicit val optionInstantGenerator: CsvGenerator[Option[Instant]] = optionGenerator
//  implicit val optionTemporalGenerator: CsvGenerator[Option[Temporal]] = optionGenerator

  /**
   * Method to return a CsvGenerator[Option[T]] whose columns are those of T.
   * Use this for an optional field whose type has several columns (e.g. a case class),
   * together with CsvRenderers.optionProductRenderer.
   * (optionGenerator gives an Option just one column.)
   *
   * @param g the CsvGenerator[T].
   * @tparam T the underlying type.
   * @return a CsvGenerator[Option[T]].
   */
  def optionProductGenerator[T](using g: CsvGenerator[T]): CsvGenerator[Option[T]] = new CsvGenerator[Option[T]] {
    val csvAttributes: CsvAttributes = g.csvAttributes

    def toColumnName(po: Option[String], name: String): String = g.toColumnName(po, name)
  }

  /**
   * Method to return a CsvGenerator[T] which does not output a column header for at all.
   *
   * @tparam T the type of the column objects.
   * @return a CsvGenerator[T].
   */
  def skipGenerator[T](using ca: CsvAttributes): CsvGenerator[T] = new CsvProductGenerator[T] {
    val csvAttributes: CsvAttributes = ca

    override def toColumnName(po: Option[String], name: String): String = ""

    // TESTME (not actually used).
    def toColumnNames(po: Option[String], no: Option[String]): String = ""
  }

// ── 14 ──────────────────────────────────────────────────────────────────────

  // ── 15 ──────────────────────────────────────────────────────────────────────

  // ── 16 ──────────────────────────────────────────────────────────────────────

  // ── 17 ──────────────────────────────────────────────────────────────────────

  // ── 18 ──────────────────────────────────────────────────────────────────────

  // ── 19 ──────────────────────────────────────────────────────────────────────

}

object CsvGenerators {
  object CsvGeneratorBoolean extends StandardCsvGenerator[Boolean]

  object CsvGeneratorInt extends StandardCsvGenerator[Int]

  object CsvGeneratorBigInt extends StandardCsvGenerator[BigInt]

  object CsvGeneratorLong extends StandardCsvGenerator[Long]

  object CsvGeneratorDouble extends StandardCsvGenerator[Double]

  object CsvGeneratorFloat extends StandardCsvGenerator[Float]

  object CsvGeneratorShort extends StandardCsvGenerator[Short]

  object CsvGeneratorByte extends StandardCsvGenerator[Byte]

  object CsvGeneratorString extends StandardCsvGenerator[String]

  object CsvGeneratorURL extends StandardCsvGenerator[URL]

  object CsvGeneratorLocalDate extends StandardCsvGenerator[LocalDate]

  object CsvGeneratorInstant extends StandardCsvGenerator[Instant]

//  object CsvGeneratorTemporal extends StandardCsvGenerator[Temporal]
}
