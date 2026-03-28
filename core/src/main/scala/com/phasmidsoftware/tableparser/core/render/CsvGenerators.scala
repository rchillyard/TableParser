package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import java.net.URL
import java.time.Instant
import org.joda.time.LocalDate
import scala.reflect.ClassTag

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
  def sequenceGenerator[T](implicit ca: CsvAttributes): CsvGenerator[Seq[T]] = new StandardCsvGenerator[Seq[T]]

  /**
   * Method to return a CsvGenerator[ Option[T] ].
   *
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[ Option[T] ].
   */
  def optionGenerator[T](implicit ca: CsvAttributes): CsvGenerator[Option[T]] = new StandardCsvGenerator[Option[T]]

  implicit val optionIntGenerator: CsvGenerator[Option[Int]] = optionGenerator
  implicit val optionDoubleGenerator: CsvGenerator[Option[Double]] = optionGenerator
  implicit val optionStringGenerator: CsvGenerator[Option[String]] = optionGenerator
  implicit val optionBooleanGenerator: CsvGenerator[Option[Boolean]] = optionGenerator
  implicit val optionLocalDateGenerator: CsvGenerator[Option[LocalDate]] = optionGenerator
  implicit val optionLongGenerator: CsvGenerator[Option[Long]] = optionGenerator
  implicit val optionFloatGenerator: CsvGenerator[Option[Float]] = optionGenerator
  implicit val optionShortGenerator: CsvGenerator[Option[Short]] = optionGenerator
  implicit val optionByteGenerator: CsvGenerator[Option[Byte]] = optionGenerator
  implicit val optionInstantGenerator: CsvGenerator[Option[Instant]] = optionGenerator
//  implicit val optionTemporalGenerator: CsvGenerator[Option[Temporal]] = optionGenerator

  /**
   * Method to return a CsvGenerator[T] which does not output a column header for at all.
   *
   * @tparam T the type of the column objects.
   * @return a CsvGenerator[T].
   */
  def skipGenerator[T](implicit ca: CsvAttributes): CsvGenerator[T] = new CsvProductGenerator[T] {
    val csvAttributes: CsvAttributes = ca

    override def toColumnName(po: Option[String], name: String): String = ""

    // TESTME (not actually used).
    def toColumnNames(po: Option[String], no: Option[String]): String = ""
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
   *
   * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
   *
   * @param construct a function P => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the (single) field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator1[P1: CsvGenerator, T <: Product : ClassTag](construct: P1 => T)(implicit c: CsvAttributes): CsvGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1) = fieldNames

    /**
     * Generates a single string representing column names by merging the specified prefixes and using a CSV generator.
     *
     * @param po An optional prefix string (e.g., for column names) to be used during the merge.
     * @param no Another optional prefix string to be used during the merge together with `po`.
     * @return A single string containing the generated column names, delimited by the specified delimiter.
     */
    def toColumnNames(po: Option[String], no: Option[String]): String = Seq(
      implicitly[CsvGenerator[P1]].toColumnName(merge(po, no), p1)
    ) mkString c.delimiter
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
   *
   * @param construct a function (P1,P2) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator2[P1: CsvGenerator, P2: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2) = fieldNames

    /**
     * Converts optional values into combined column names, utilizing CSV generators
     * for predefined types, and joins them using a specified delimiter.
     *
     * @param po An optional string representing the first value.
     * @param no An optional string representing the second value.
     * @return A string containing the generated column names, concatenated with a delimiter.
     */
    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
   *
   * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator3[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3) = fieldNames

    /**
     * Generates a delimited string of column names based on the provided optional parameters.
     *
     * @param po An optional string parameter which represents a partial value for generating column names.
     * @param no An optional string parameter which represents another partial value to be merged for column name generation.
     * @return A string containing the concatenated column names separated by a specified delimiter.
     */
    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 4-ary Product and which is based on a function to convert a (P1,P2,P3,P4) into a T.
   *
   * @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator4[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 5-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator5[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 6-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator6[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 7-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator7[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is an 8-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam P8 the type of the eighth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator8[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 9-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam P8 the type of the eighth field of the Product type T.
   * @tparam P9 the type of the ninth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator9[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 10-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator10[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is an 11-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator11[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 12-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator12[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
      ) mkString c.delimiter
    }
  }

  /**
   * Method to return a CsvGenerator[T] where T is a 13-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13) into a T.
   *
   * @author IntelliJ AI Assistant
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator13[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
      ) mkString c.delimiter
    }
  }
// ── 14 ──────────────────────────────────────────────────────────────────────

  /**
   * Method to return a CsvGenerator[T] where T is a 14-ary Product and which is based on a function to convert a
   * (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14) into a T.
   *
   * @param construct a function (P1,...,P14) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam P14 the type of the fourteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator14[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, P14: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
        , implicitly[CsvGenerator[P14]].toColumnName(wo, p14)
      ) mkString c.delimiter
    }
  }

  // ── 15 ──────────────────────────────────────────────────────────────────────

  /**
   * Method to return a CsvGenerator[T] where T is a 15-ary Product and which is based on a function to convert a
   * (P1,...,P15) into a T.
   *
   * @param construct a function (P1,...,P15) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam P14 the type of the fourteenth field of the Product type T.
   * @tparam P15 the type of the fifteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator15[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, P14: CsvGenerator, P15: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
        , implicitly[CsvGenerator[P14]].toColumnName(wo, p14)
        , implicitly[CsvGenerator[P15]].toColumnName(wo, p15)
      ) mkString c.delimiter
    }
  }

  // ── 16 ──────────────────────────────────────────────────────────────────────

  /**
   * Method to return a CsvGenerator[T] where T is a 16-ary Product and which is based on a function to convert a
   * (P1,...,P16) into a T.
   *
   * @param construct a function (P1,...,P16) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam P14 the type of the fourteenth field of the Product type T.
   * @tparam P15 the type of the fifteenth field of the Product type T.
   * @tparam P16 the type of the sixteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator16[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, P14: CsvGenerator, P15: CsvGenerator, P16: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
        , implicitly[CsvGenerator[P14]].toColumnName(wo, p14)
        , implicitly[CsvGenerator[P15]].toColumnName(wo, p15)
        , implicitly[CsvGenerator[P16]].toColumnName(wo, p16)
      ) mkString c.delimiter
    }
  }

  // ── 17 ──────────────────────────────────────────────────────────────────────

  /**
   * Method to return a CsvGenerator[T] where T is a 17-ary Product and which is based on a function to convert a
   * (P1,...,P17) into a T.
   *
   * @param construct a function (P1,...,P17) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam P14 the type of the fourteenth field of the Product type T.
   * @tparam P15 the type of the fifteenth field of the Product type T.
   * @tparam P16 the type of the sixteenth field of the Product type T.
   * @tparam P17 the type of the seventeenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator17[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, P14: CsvGenerator, P15: CsvGenerator, P16: CsvGenerator, P17: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
        , implicitly[CsvGenerator[P14]].toColumnName(wo, p14)
        , implicitly[CsvGenerator[P15]].toColumnName(wo, p15)
        , implicitly[CsvGenerator[P16]].toColumnName(wo, p16)
        , implicitly[CsvGenerator[P17]].toColumnName(wo, p17)
      ) mkString c.delimiter
    }
  }

  // ── 18 ──────────────────────────────────────────────────────────────────────

  /**
   * Method to return a CsvGenerator[T] where T is an 18-ary Product and which is based on a function to convert a
   * (P1,...,P18) into a T.
   *
   * @param construct a function (P1,...,P18) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam P14 the type of the fourteenth field of the Product type T.
   * @tparam P15 the type of the fifteenth field of the Product type T.
   * @tparam P16 the type of the sixteenth field of the Product type T.
   * @tparam P17 the type of the seventeenth field of the Product type T.
   * @tparam P18 the type of the eighteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator18[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, P14: CsvGenerator, P15: CsvGenerator, P16: CsvGenerator, P17: CsvGenerator, P18: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
        , implicitly[CsvGenerator[P14]].toColumnName(wo, p14)
        , implicitly[CsvGenerator[P15]].toColumnName(wo, p15)
        , implicitly[CsvGenerator[P16]].toColumnName(wo, p16)
        , implicitly[CsvGenerator[P17]].toColumnName(wo, p17)
        , implicitly[CsvGenerator[P18]].toColumnName(wo, p18)
      ) mkString c.delimiter
    }
  }

  // ── 19 ──────────────────────────────────────────────────────────────────────

  /**
   * Method to return a CsvGenerator[T] where T is a 19-ary Product and which is based on a function to convert a
   * (P1,...,P19) into a T.
   *
   * @param construct a function (P1,...,P19) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the third field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam P14 the type of the fourteenth field of the Product type T.
   * @tparam P15 the type of the fifteenth field of the Product type T.
   * @tparam P16 the type of the sixteenth field of the Product type T.
   * @tparam P17 the type of the seventeenth field of the Product type T.
   * @tparam P18 the type of the eighteenth field of the Product type T.
   * @tparam P19 the type of the nineteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvGenerator[T].
   */
  def generator19[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, P13: CsvGenerator, P14: CsvGenerator, P15: CsvGenerator, P16: CsvGenerator, P17: CsvGenerator, P18: CsvGenerator, P19: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new StandardCsvGenerator[T]() with CsvProductGenerator[T] {
    private val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) = fieldNames

    def toColumnNames(po: Option[String], no: Option[String]): String = {
      val wo = merge(po, no)
      Seq(
        implicitly[CsvGenerator[P1]].toColumnName(wo, p1)
        , implicitly[CsvGenerator[P2]].toColumnName(wo, p2)
        , implicitly[CsvGenerator[P3]].toColumnName(wo, p3)
        , implicitly[CsvGenerator[P4]].toColumnName(wo, p4)
        , implicitly[CsvGenerator[P5]].toColumnName(wo, p5)
        , implicitly[CsvGenerator[P6]].toColumnName(wo, p6)
        , implicitly[CsvGenerator[P7]].toColumnName(wo, p7)
        , implicitly[CsvGenerator[P8]].toColumnName(wo, p8)
        , implicitly[CsvGenerator[P9]].toColumnName(wo, p9)
        , implicitly[CsvGenerator[P10]].toColumnName(wo, p10)
        , implicitly[CsvGenerator[P11]].toColumnName(wo, p11)
        , implicitly[CsvGenerator[P12]].toColumnName(wo, p12)
        , implicitly[CsvGenerator[P13]].toColumnName(wo, p13)
        , implicitly[CsvGenerator[P14]].toColumnName(wo, p14)
        , implicitly[CsvGenerator[P15]].toColumnName(wo, p15)
        , implicitly[CsvGenerator[P16]].toColumnName(wo, p16)
        , implicitly[CsvGenerator[P17]].toColumnName(wo, p17)
        , implicitly[CsvGenerator[P18]].toColumnName(wo, p18)
        , implicitly[CsvGenerator[P19]].toColumnName(wo, p19)
      ) mkString c.delimiter
    }
  }
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
