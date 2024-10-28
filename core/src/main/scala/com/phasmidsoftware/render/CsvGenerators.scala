package com.phasmidsoftware.render

import com.phasmidsoftware.table.CsvAttributes
import java.net.URL
import scala.reflect.ClassTag

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
   * @tparam P4 the type of the third field of the Product type T.
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
   * Method to return a CsvGenerator[T] where T is a 8-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8) into a T.
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
   * Method to return a CsvGenerator[T] where T is a 11-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) into a T.
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
}

object CsvGenerators {
  implicit object CsvGeneratorBoolean extends StandardCsvGenerator[Boolean]

  implicit object CsvGeneratorInt extends StandardCsvGenerator[Int]

  implicit object CsvGeneratorBigInt extends StandardCsvGenerator[BigInt]

  implicit object CsvGeneratorLong extends StandardCsvGenerator[Long]

  implicit object CsvGeneratorDouble extends StandardCsvGenerator[Double]

  implicit object CsvGeneratorString extends StandardCsvGenerator[String]

  implicit object CsvGeneratorURL extends StandardCsvGenerator[URL]
}
