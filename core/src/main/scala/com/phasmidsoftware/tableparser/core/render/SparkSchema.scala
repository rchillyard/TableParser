package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.LineParser
import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror
import scala.util.Try

/**
 * Type class for the Spark SQL type of a single column (#117): for example, Int is INT and Instant is TIMESTAMP.
 * Spark itself isn't a dependency: the type is just its name, as Spark's DDL writes it.
 *
 * @tparam T the type of a field.
 */
trait SparkType[T]:
  /**
   * @return the Spark SQL type, as it appears in a DDL schema, such as "INT" or "DECIMAL(38,18)".
   */
  def sqlType: String

/**
 * Companion object for SparkType: the types of the fields which CellParser can parse.
 *
 * A java.time type maps to the Spark type which reads its ISO-8601 form (as CsvRenderer writes it):
 * Instant and OffsetDateTime to TIMESTAMP, LocalDateTime to TIMESTAMP_NTZ (no time zone), LocalDate to DATE.
 * Spark can't read a ZonedDateTime's region (such as [America/New_York]), nor has every Spark version a TIME type,
 * so ZonedDateTime and LocalTime are STRING. For another field type, give a SparkType of your own.
 */
object SparkType:
  /**
   * Method to make a SparkType.
   *
   * @param w the Spark SQL type.
   * @tparam T the field type.
   * @return a SparkType[T].
   */
  def apply[T](w: String): SparkType[T] = new SparkType[T]:
    val sqlType: String = w

  given SparkTypeString: SparkType[String] = SparkType("STRING")
  given SparkTypeBoolean: SparkType[Boolean] = SparkType("BOOLEAN")
  given SparkTypeByte: SparkType[Byte] = SparkType("TINYINT")
  given SparkTypeShort: SparkType[Short] = SparkType("SMALLINT")
  given SparkTypeInt: SparkType[Int] = SparkType("INT")
  given SparkTypeLong: SparkType[Long] = SparkType("BIGINT")
  given SparkTypeFloat: SparkType[Float] = SparkType("FLOAT")
  given SparkTypeDouble: SparkType[Double] = SparkType("DOUBLE")
  given SparkTypeBigInt: SparkType[BigInt] = SparkType("DECIMAL(38,0)")
  given SparkTypeBigDecimal: SparkType[BigDecimal] = SparkType("DECIMAL(38,18)")
  given SparkTypeLocalDate: SparkType[LocalDate] = SparkType("DATE")
  given SparkTypeInstant: SparkType[Instant] = SparkType("TIMESTAMP")
  given SparkTypeLocalDateTime: SparkType[LocalDateTime] = SparkType("TIMESTAMP_NTZ")
  given SparkTypeOffsetDateTime: SparkType[OffsetDateTime] = SparkType("TIMESTAMP")
  given SparkTypeZonedDateTime: SparkType[ZonedDateTime] = SparkType("STRING")
  given SparkTypeLocalTime: SparkType[LocalTime] = SparkType("STRING")
  given SparkTypeURL: SparkType[java.net.URL] = SparkType("STRING")

  /**
   * An optional field has the type of its value: Spark's columns are nullable, and an empty cell is read as null.
   */
  given SparkTypeOption[T](using t: SparkType[T]): SparkType[Option[T]] = SparkType(t.sqlType)

/**
 * Type class for the Spark SQL types of the columns of a row type T, in the order of its CSV columns (#117).
 *
 * Use SparkSchema.ddl to get the schema of a CSV message (as IOKafka.toKafka and ZIOKafka.toKafka send it)
 * for Spark's from_csv, which matches columns by position:
 * the column names are those of CsvGenerator.header (so they agree with the tableparser.columns record header),
 * and each column's type is that of its field.
 *
 * @tparam T the row type.
 */
trait SparkSchema[T]:
  /**
   * @return the Spark SQL type of each column, in order (a field which is a case class gives one type for each of its columns).
   */
  def sqlTypes: List[String]

/**
 * Companion object for SparkSchema.
 */
object SparkSchema:
  /**
   * Method to get the Spark DDL schema of the CSV form of T, such as "`VendorID` INT, `pickup` TIMESTAMP".
   * Each name is in backticks (so it may contain any character), and comes from CsvGenerator.header[T].
   *
   * @param schema    the (given) SparkSchema[T].
   * @param generator the (given) CsvGenerator[T], which names the columns.
   * @param ca        the (given) CsvAttributes, with which the header is written.
   * @tparam T the row type.
   * @return a Try of the schema: a Failure if the number of column names isn't the number of types
   *         (if a field has a CsvGenerator of its own which doesn't match the way its SparkType is found).
   */
  def ddl[T](using schema: SparkSchema[T], generator: CsvGenerator[T], ca: CsvAttributes): Try[String] =
    for
      names <- LineParser(using ca.rowConfig).parseRow((CsvGenerator.header[T], 0))
      _ <- Try(require(names.size == schema.sqlTypes.size, s"SparkSchema.ddl: ${names.size} columns (${names.mkString(", ")}) but ${schema.sqlTypes.size} types"))
    yield names.zip(schema.sqlTypes).map((n, t) => s"`${n.replace("`", "``")}` $t").mkString(", ")

  /**
   * Method to derive a SparkSchema for a case class T, so that `case class T(...) derives SparkSchema` works.
   * Each field gives its SparkType; a field whose type is a case class (with no SparkType of its own)
   * gives the types of its own fields in turn, as CsvGenerator.derived gives their columns.
   *
   * @param m the Mirror of T.
   * @tparam T a case class.
   * @return a SparkSchema[T].
   */
  inline def derived[T](using m: Mirror.ProductOf[T]): SparkSchema[T] =
    apply[T](fieldTypes[m.MirroredElemTypes])

  /**
   * Method to make a SparkSchema from the types of its columns.
   *
   * @param types the Spark SQL type of each column, in order.
   * @tparam T the row type.
   * @return a SparkSchema[T].
   */
  def apply[T](types: List[String]): SparkSchema[T] = new SparkSchema[T]:
    val sqlTypes: List[String] = types

  private inline def fieldTypes[Ts <: Tuple]: List[String] = inline erasedValue[Ts] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => fieldType[t] ++ fieldTypes[ts]

  // Prefer a field type's own SparkType; otherwise, if it is a case class, derive its types.
  private inline def fieldType[A]: List[String] = summonFrom:
    case t: SparkType[A] => List(t.sqlType)
    case m: Mirror.ProductOf[A] => derived[A](using m).sqlTypes
