package com.phasmidsoftware.parse

import com.phasmidsoftware.table._

import scala.reflect.ClassTag
import scala.util.Try

/**
  * Trait to define the various parsers for reading case classes and their parameters from table rows.
  *
  * NOTE In each of these cellParser methods, the CellParser has a parse method which ignores the columns.
  *
  */
trait CellParsers {

  /**
    * Method to return a CellParser[Seq[P].
    * This is used only by unit tests.
    * CONSIDER eliminating this; only used in unit tests
    *
    * @tparam P the underlying type of the result
    * @return a MultiCellParser[Seq[P]
    */
  def cellParserSeq[P: CellParser]: CellParser[Seq[P]] = {
    new MultiCellParser[Seq[P]] {
      override def toString: String = "MultiCellParser: cellParserSeq"

      def parse(w: Option[String], row: Row, columns: Header): Seq[P] = for (w <- row.ws) yield implicitly[CellParser[P]].parse(CellValue(w))
    }
  }

  /**
    * Method to return a CellParser[Option[P].
    *
    * @tparam P the underlying type of the result
    * @return a MultiCellParser[Option[P]
    */
  def cellParserOption[P: CellParser]: CellParser[Option[P]] = {
    new SingleCellParser[Option[P]] {
      override def toString: String = s"cellParserOption with $cp"

      private val cp: CellParser[P] = implicitly[CellParser[P]]

      def convertString(w: String): Option[P] = Try(cp.parse(CellValue(w))).toOption

      override def parse(wo: Option[String], row: Row, columns: Header): Option[P] = Try(cp.parse(wo, row, columns)).toOption

    }
  }

  /**
    * Method to return a CellParser[T] based on a function to convert a P into a T
    *
    * @param construct a function P => T.
    * @tparam P the type of the intermediate type.
    * @tparam T the underlying type of the result.
    * @return a SingleCellParser which converts a String into the intermediate type P and thence into a T
    */
  def cellParser[P: CellParser, T: ClassTag](construct: P => T): CellParser[T] = {
    new SingleCellParser[T] {
      override def toString: String = s"SingleCellParser for ${implicitly[ClassTag[T]]}"

      def convertString(w: String): T = construct(implicitly[CellParser[P]].parse(CellValue(w)))
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
    * It probably shouldn't ever be used in practice. It can cause strange initialization errors!
    * This note may be irrelevant now that we have overridden convertString to fix issue #1.
    *
    * @param construct a function P => T, usually the apply method of a case class.
    * @tparam P1 the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts a String from a Row into the field type P and thence into a T
    */
  def cellParser1[P1: CellParser, T <: Product : ClassTag : ColumnHelper](construct: P1 => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser1 for $tc"

      def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        construct(p1V)
      }

      // We need to allow for a single-parameter conversion of String => T via P1
      // This fixes issue #1
      override def convertString(w: String): T = construct(implicitly[CellParser[P1]].convertString(w))
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
    *
    * @param construct a function (P1,P2) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1 and P2 and thence into a T
    */
  def cellParser2[P1: CellParser, P2: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser2 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        construct(p1V, p2V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
    *
    * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2 and P3 and thence into a T
    */
  def cellParser3[P1: CellParser, P2: CellParser, P3: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser3 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        construct(p1V, p2V, p3V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 4-ary Product and which is based on a function to convert a (P1,P2,P3,P4) into a T.
    *
    * @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3 and P4 and thence into a T
    */
  def cellParser4[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser4 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        construct(p1V, p2V, p3V, p4V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 5-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam P5 the type of the fifth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4 and P5 and thence into a T
    */
  def cellParser5[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser5 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        construct(p1V, p2V, p3V, p4V, p5V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 6-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam P5 the type of the fifth field of the Product type T.
    * @tparam P6 the type of the sixth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5 and P6 and thence into a T
    */
  def cellParser6[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser6 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        construct(p1V, p2V, p3V, p4V, p5V, p6V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 7-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam P5 the type of the fifth field of the Product type T.
    * @tparam P6 the type of the sixth field of the Product type T.
    * @tparam P7 the type of the seventh field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6 and P7 and thence into a T
    */
  def cellParser7[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser7 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        val p7V = readCell[T, P7](wo, row, columns)(p7)
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 8-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam P5 the type of the fifth field of the Product type T.
    * @tparam P6 the type of the sixth field of the Product type T.
    * @tparam P7 the type of the seventh field of the Product type T.
    * @tparam P8 the type of the eighth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7 and P8 and thence into a T
    */
  def cellParser8[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser8 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        val p7V = readCell[T, P7](wo, row, columns)(p7)
        val p8V = readCell[T, P8](wo, row, columns)(p8)
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 9-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam P5 the type of the fifth field of the Product type T.
    * @tparam P6 the type of the sixth field of the Product type T.
    * @tparam P7 the type of the seventh field of the Product type T.
    * @tparam P8 the type of the eighth field of the Product type T.
    * @tparam P9 the type of the ninth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8 and P9 and thence into a T
    */
  def cellParser9[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser9 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        val p7V = readCell[T, P7](wo, row, columns)(p7)
        val p8V = readCell[T, P8](wo, row, columns)(p8)
        val p9V = readCell[T, P9](wo, row, columns)(p9)
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 10-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => T, usually the apply method of a case class.
    * @tparam P1  the type of the first field of the Product type T.
    * @tparam P2  the type of the second field of the Product type T.
    * @tparam P3  the type of the second field of the Product type T.
    * @tparam P4  the type of the fourth field of the Product type T.
    * @tparam P5  the type of the fifth field of the Product type T.
    * @tparam P6  the type of the sixth field of the Product type T.
    * @tparam P7  the type of the seventh field of the Product type T.
    * @tparam P8  the type of the eighth field of the Product type T.
    * @tparam P9  the type of the ninth field of the Product type T.
    * @tparam P10 the type of the tenth field of the Product type T.
    * @tparam T   the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8, P9 and P10 and thence into a T
    */
  def cellParser10[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, P10: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser10 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        val p7V = readCell[T, P7](wo, row, columns)(p7)
        val p8V = readCell[T, P8](wo, row, columns)(p8)
        val p9V = readCell[T, P9](wo, row, columns)(p9)
        val p10V = readCell[T, P10](wo, row, columns)(p10)
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V, p10V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 11-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
    * @tparam P1  the type of the first field of the Product type T.
    * @tparam P2  the type of the second field of the Product type T.
    * @tparam P3  the type of the second field of the Product type T.
    * @tparam P4  the type of the fourth field of the Product type T.
    * @tparam P5  the type of the fifth field of the Product type T.
    * @tparam P6  the type of the sixth field of the Product type T.
    * @tparam P7  the type of the seventh field of the Product type T.
    * @tparam P8  the type of the eighth field of the Product type T.
    * @tparam P9  the type of the ninth field of the Product type T.
    * @tparam P10 the type of the tenth field of the Product type T.
    * @tparam P11 the type of the eleventh field of the Product type T.
    * @tparam T   the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 and P11 and thence into a T
    */
  def cellParser11[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, P10: CellParser, P11: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser11 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        val p7V = readCell[T, P7](wo, row, columns)(p7)
        val p8V = readCell[T, P8](wo, row, columns)(p8)
        val p9V = readCell[T, P9](wo, row, columns)(p9)
        val p10V = readCell[T, P10](wo, row, columns)(p10)
        val p11V = readCell[T, P11](wo, row, columns)(p11)
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V, p10V, p11V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 12-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => T, usually the apply method of a case class.
    * @tparam P1  the type of the first field of the Product type T.
    * @tparam P2  the type of the second field of the Product type T.
    * @tparam P3  the type of the second field of the Product type T.
    * @tparam P4  the type of the fourth field of the Product type T.
    * @tparam P5  the type of the fifth field of the Product type T.
    * @tparam P6  the type of the sixth field of the Product type T.
    * @tparam P7  the type of the seventh field of the Product type T.
    * @tparam P8  the type of the eighth field of the Product type T.
    * @tparam P9  the type of the ninth field of the Product type T.
    * @tparam P10 the type of the tenth field of the Product type T.
    * @tparam P11 the type of the eleventh field of the Product type T.
    * @tparam P12 the type of the twelfth field of the Product type T.
    * @tparam T   the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11 and P12 and thence into a T
    */
  def cellParser12[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, P10: CellParser, P11: CellParser, P12: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = CellParsers.extractFieldNames(tc)
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser12 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): T = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        val p2V = readCell[T, P2](wo, row, columns)(p2)
        val p3V = readCell[T, P3](wo, row, columns)(p3)
        val p4V = readCell[T, P4](wo, row, columns)(p4)
        val p5V = readCell[T, P5](wo, row, columns)(p5)
        val p6V = readCell[T, P6](wo, row, columns)(p6)
        val p7V = readCell[T, P7](wo, row, columns)(p7)
        val p8V = readCell[T, P8](wo, row, columns)(p8)
        val p9V = readCell[T, P9](wo, row, columns)(p9)
        val p10V = readCell[T, P10](wo, row, columns)(p10)
        val p11V = readCell[T, P11](wo, row, columns)(p11)
        val p12V = readCell[T, P12](wo, row, columns)(p12)
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V, p10V, p11V, p12V)
      }
    }
  }

  def columnHelper[T](columnNameMapper: String => String, maybePrefix: Option[String], aliases: (String, String)*): ColumnHelper[T] = new ColumnHelper[T] {
    override val _maybePrefix: Option[String] = maybePrefix
    override val _aliases: Seq[(String, String)] = aliases
    override val _columnNameMapper: String => String = columnNameMapper
  }

  def columnHelper[T](columnNameMapper: String => String, aliases: (String, String)*): ColumnHelper[T] = columnHelper(columnNameMapper, None, aliases: _*)

  def columnHelper[T](maybePrefix: Option[String], aliases: (String, String)*): ColumnHelper[T] = columnHelper(identity[String] _, maybePrefix, aliases: _*)

  def columnHelper[T](aliases: (String, String)*): ColumnHelper[T] = columnHelper(identity[String] _, aliases: _*)

  private def readCell[T <: Product : ClassTag : ColumnHelper, P: CellParser](wo: Option[String], row: Row, columns: Header)(p: String): P = {
    val columnName = implicitly[ColumnHelper[T]].lookup(wo, p)
    val cellParser = implicitly[CellParser[P]]
    val idx = row.getIndex(columnName)
    //   println(s"readCell[${implicitly[ClassTag[T]].runtimeClass}](wo=$wo,...)($p) with cellParser=$cellParser, ids=$idx")
    if (idx >= 0) try cellParser.parse(CellValue(row(idx))) catch {
      case e: Exception => throw ParserException(s"Problem parsing '${row(idx)}' as ${implicitly[ClassTag[T]].runtimeClass} from $columnName at index $idx of $row", e)
    }
    else try cellParser.parse(Some(columnName), row, columns) catch {
      case _: UnsupportedOperationException => throw ParserException(s"unable to find value for column $columnName")
    }
  }
}

/**
  * This companion object comprises CellParser[T] objects which represent conversions that are fixed,
  * i.e. they don't depend on some other parameter such as the formatter in DateTime conversions.
  */
object CellParsers {

  private def extractFieldNames(classTag: ClassTag[_]): Array[String] = {
    import java.lang.reflect.Modifier

    import scala.util.control.NonFatal

    val clazz = classTag.runtimeClass
    try {
      // copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
      // that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
      val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
        _.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)
      val fields = clazz.getDeclaredFields.filterNot { f =>
        import Modifier._
        (f.getModifiers & (TRANSIENT | STATIC | 0x1000 /* SYNTHETIC*/)) > 0
      }
      if (copyDefaultMethods.length != fields.length)
        sys.error("Case class " + clazz.getName + " declares additional fields")
      if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
        sys.error("Cannot determine field order of case class " + clazz.getName)
      fields.map(f => f.getName)
    } catch {
      case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
        "for '" + clazz.getName + "', please use the 'jsonFormat' overload with explicit field name specification", ex)
    }
  }

}

case class ParsersException(w: String) extends Exception(w)



/**
  * This class is used for the situation where a column in a table actually contains a set of
  * attributes, typically separated by "|" and possibly bracketed by "{}"
  *
  * @param xs the attribute values.
  */
case class AttributeSet(xs: StringList)

object AttributeSet {
  def apply(w: String): AttributeSet = apply(Parseable.split(w))
}

