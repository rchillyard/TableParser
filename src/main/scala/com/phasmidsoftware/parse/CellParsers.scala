/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table._
import com.phasmidsoftware.util.{FP, Reflection}

import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Trait to define the various parsers for reading case classes and their parameters from table rows.
  *
  * NOTE In each of these cellParser methods, the CellParser has a parse method which ignores the columns.
  *
  */
trait CellParsers {

  /**
    * Method to return a CellParser[Seq[P] from a potentially unlimited set of P objects.
    * The counting of the elements starts at start (defaults to 1).
    *
    * @tparam P the underlying type of the result
    * @return a MultiCellParser[Seq[P]
    */
  def cellParserRepetition[P: CellParser : ColumnHelper](start: Int = 1): CellParser[Seq[P]] = new MultiCellParser[Seq[P]] {
    override def toString: String = "MultiCellParser: cellParserRepetition"

    def parse(wo: Option[String], row: Row, columns: Header): Try[Seq[P]] = {
      def getTryP(i: Int) = implicitly[CellParser[P]].parse(Some(s"$i"), row, columns)

      FP.sequence(LazyList.from(start).map(getTryP).takeWhile(_.isSuccess).toList)
    }
  }

  /**
    * Method to return a CellParser[Seq[P].
    * This is used only by unit tests.
    * CONSIDER eliminating this; only used in unit tests
    *
    * @tparam P the underlying type of the result
    * @return a MultiCellParser[Seq[P]
    */
  def cellParserSeq[P: CellParser]: CellParser[Seq[P]] = new MultiCellParser[Seq[P]] {
    override def toString: String = "MultiCellParser: cellParserSeq"

    def parse(wo: Option[String], row: Row, columns: Header): Try[Seq[P]] = FP.sequence(for (w <- row.ws) yield implicitly[CellParser[P]].parse(CellValue(w)))
  }

  /**
    * Method to return a CellParser[Option[P].
    *
    * This class is used for optional types which are non-scalar, i.e. there are no implicitly-defined parsers for the Option[P].
    *
    * @tparam P the underlying type of the result
    * @return a SingleCellParser[Option[P]
    */
  def cellParserOption[P: CellParser]: CellParser[Option[P]] = new SingleCellParser[Option[P]] {
    override def toString: String = s"cellParserOption with $cp"

    private val cp: CellParser[P] = implicitly[CellParser[P]]

    def convertString(w: String): Try[Option[P]] = convertToOptionP(cp.parse(CellValue(w)))

    override def parse(wo: Option[String], row: Row, columns: Header): Try[Option[P]] = convertToOptionP(cp.parse(wo, row, columns))

    private def convertToOptionP(py: Try[P]) = py.map(Some(_)).recoverWith {
      case _: ParseableException => Success(None)
    }
  }

  /**
    * Method to return a CellParser[Option[String].
    *
    * TODO: why do we need this in addition to cellParserOption?
    *
    * CONSIDER: using BlankException as above...
    *
    * @return a SingleCellParser[Option[String]
    */
  def cellParserOptionNonEmptyString: CellParser[Option[String]] = new SingleCellParser[Option[String]] {
    override def toString: String = s"cellParserOptionNonEmptyString"

    private val cp: CellParser[String] = new CellParser[String]() {
      def convertString(w: String): Try[String] = Success(if (w.isEmpty) null else w)

      override def parse(wo: Option[String], row: Row, columns: Header): Try[String] = Failure(new UnsupportedOperationException)
    }

    def convertString(w: String): Try[Option[String]] = cp.convertString(w).map(Option(_))

    override def parse(wo: Option[String], row: Row, columns: Header): Try[Option[String]] = cp.parse(wo, row, columns).map(Option(_))
  }

  /**
    * Method to return a CellParser[T] based on a function to convert a P into a T
    *
    * @param construct a function P => T.
    * @tparam P the type of the intermediate type.
    * @tparam T the underlying type of the result.
    * @return a SingleCellParser which converts a String into the intermediate type P and thence into a T
    */
  def cellParser[P: CellParser, T: ClassTag](construct: P => T): CellParser[T] = new SingleCellParser[T] {
    override def toString: String = s"SingleCellParser for ${implicitly[ClassTag[T]]}"

    def convertString(w: String): Try[T] = implicitly[CellParser[P]].parse(CellValue(w)).map(construct)
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
  def cellParser1[P1: CellParser, T <: Product : ClassTag : ColumnHelper](construct: P1 => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }

    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser1 for $tc"

      def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        val p1V = readCell[T, P1](wo, row, columns)(p1)
        p1V.map(construct)
      }

      override def convertString(w: String): Try[T] = implicitly[CellParser[P1]].convertString(w).map(construct)
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
  def cellParser2[P1: CellParser, P2: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2) => T, fields: Seq[String] = Nil): CellParser[T] = {
    // CONSIDER refactoring all the repetitive code here (a macro, perhaps?)
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser2 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for (p1V <- readP[P1](p1); p2V <- readP[P2](p2)) yield construct(p1V, p2V)
      }
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 2-ary Product and which is based on a function to convert a (K,P) into a T.
    * This method differs from cellParser2 in that the parser of P (a CellParser[P]) is not found implicitly, but rather is looked up
    * dynamically depending on the value of the first parameter (of type K).
    *
    * @param construct a function (K,P) => T, usually the apply method of a case class.
    * @param parsers   a Map[K, CellParser[P] ] which determines which particular parser of P will be used.
    *                  The key value looked up is the value of the first (K) field.
    * @tparam K the type of the conditional lookup key, which is also the type of the first field of T.
    *           Typically, this will be a String, but it could also be an Int or something more exotic.
    * @tparam P the type of the second field of the Product type T.
    * @tparam T the underlying type of the result, a Product.
    * @return a MultiCellParser which converts Strings from a Row into the field types K and P and thence into a T.
    * @throws NoSuchElementException if the key (from the first K-type parameter) is not present in the map given by parsers.
    */
  def cellParser2Conditional[K: CellParser, P, T <: Product : ClassTag : ColumnHelper](construct: (K, P) => T, parsers: Map[K, CellParser[P]], fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser2 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        for {
          k <- readCell[T, K](wo, row, columns)(p1)
          p <- readCell[T, P](wo, row, columns)(p2)(tc, implicitly[ColumnHelper[T]], parsers(k))
        } yield construct(k, p)
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
  def cellParser3[P1: CellParser, P2: CellParser, P3: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser3 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
        } yield construct(p1V, p2V, p3V)
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
  def cellParser4[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser4 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
        } yield construct(p1V, p2V, p3V, p4V)
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
  def cellParser5[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser5 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
        } yield construct(p1V, p2V, p3V, p4V, p5V)
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
  def cellParser6[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser6 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V)
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
  def cellParser7[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser7 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
          p7V <- readP[P7](p7)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V)
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
  def cellParser8[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser8 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
          p7V <- readP[P7](p7)
          p8V <- readP[P8](p8)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V)
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
  def cellParser9[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser9 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
          p7V <- readP[P7](p7)
          p8V <- readP[P8](p8)
          p9V <- readP[P9](p9)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V)
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
  def cellParser10[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, P10: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser10 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
          p7V <- readP[P7](p7)
          p8V <- readP[P8](p8)
          p9V <- readP[P9](p9)
          p10V <- readP[P10](p10)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V, p10V)
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
  def cellParser11[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, P10: CellParser, P11: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser11 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
          p7V <- readP[P7](p7)
          p8V <- readP[P8](p8)
          p9V <- readP[P9](p9)
          p10V <- readP[P10](p10)
          p11V <- readP[P11](p11)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V, p10V, p11V)
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
  def cellParser12[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, P8: CellParser, P9: CellParser, P10: CellParser, P11: CellParser, P12: CellParser, T <: Product : ClassTag : ColumnHelper](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T, fields: Seq[String] = Nil): CellParser[T] = {
    val tc = implicitly[ClassTag[T]]
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = fields match {
      case Nil => Reflection.extractFieldNames(tc)
      case ps => ps.toArray
    }
    new MultiCellParser[T] {
      override def toString: String = s"MultiCellParser: cellParser12 for $tc"

      override def parse(wo: Option[String], row: Row, columns: Header): Try[T] = {
        def readP[P: CellParser](w: String): Try[P] = readCell[T, P](wo, row, columns)(w)

        for {
          p1V <- readP[P1](p1)
          p2V <- readP[P2](p2)
          p3V <- readP[P3](p3)
          p4V <- readP[P4](p4)
          p5V <- readP[P5](p5)
          p6V <- readP[P6](p6)
          p7V <- readP[P7](p7)
          p8V <- readP[P8](p8)
          p9V <- readP[P9](p9)
          p10V <- readP[P10](p10)
          p11V <- readP[P11](p11)
          p12V <- readP[P12](p12)
        } yield construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V, p8V, p9V, p10V, p11V, p12V)
      }
    }
  }

  /**
    * Method to yield a ColumnHelper[T] based on an optional prefix, and some number of explicit aliases,
    *
    * @param maybePrefix an optional prefix for the column name.
    * @param aliases     a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
    * @tparam T the underlying type of the resulting ColumnHelper
    * @return a new instance of ColumnHelper[T]
    */
  def columnHelper[T](maybePrefix: Option[String], aliases: (String, String)*): ColumnHelper[T] = columnHelper(identity[String] _, maybePrefix, aliases: _*)

  /**
    * Method to yield a ColumnHelper[T] based on a column name mapper, and some number of explicit aliases,
    *
    * @param columnNameMapper a mapper of String=>String which will translate case class parameter names into column names.
    * @param aliases          a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
    * @tparam T the underlying type of the resulting ColumnHelper
    * @return a new instance of ColumnHelper[T]
    */
  def columnHelper[T](columnNameMapper: String => String, aliases: (String, String)*): ColumnHelper[T] = columnHelper(columnNameMapper, None, aliases: _*)

  /**
    * Method to yield a ColumnHelper[T] based on a column name mapper, an optional prefix, and some number of explicit aliases,
    *
    * @param columnNameMapper a mapper of String=>String which will translate case class parameter names into column names.
    * @param maybePrefix      an optional prefix for the column name.
    * @param aliases          a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
    * @tparam T the underlying type of the resulting ColumnHelper
    * @return a new instance of ColumnHelper[T]
    */
  def columnHelper[T](columnNameMapper: String => String, maybePrefix: Option[String], aliases: (String, String)*): ColumnHelper[T] = new ColumnHelper[T] {
    override val maybePrefix_ : Option[String] = maybePrefix
    override val aliases_ : Seq[(String, String)] = aliases
    override val columnNameMapper_ : String => String = columnNameMapper
  }

  /**
    * Method to yield a ColumnHelper[T] based on  some number of explicit aliases,
    *
    * @param aliases a variable number of explicit aliases to translate specific case class parameter names into their column name equivalents.
    * @tparam T the underlying type of the resulting ColumnHelper
    * @return a new instance of ColumnHelper[T]
    */
  def columnHelper[T](aliases: (String, String)*): ColumnHelper[T] = columnHelper(identity[String] _, aliases: _*)

  /**
    * A default column mapper which will work for any underlying type T and which provides no column name mapping at all.
    * This is suitable for the usual case where the names of the, e.g., CSV columns are the same as the names of the case class parameters.
    *
    * @tparam T the underlying type of the resulting ColumnHelper
    * @return a new instance of ColumnHelper[T]
    */
  implicit def defaultColumnHelper[T]: ColumnHelper[T] = columnHelper()

  // CONSIDER inlining readCellWithHeader
  private def readCell[T <: Product : ClassTag : ColumnHelper, P: CellParser](wo: Option[String], row: Row, columns: Header)(p: String): Try[P] = // if (columns.exists)
    readCellWithHeader(wo, row, columns, p)

  private def readCellWithHeader[P: CellParser, T <: Product : ClassTag : ColumnHelper](wo: Option[String], row: Row, columns: Header, p: String) = {
    val columnName = implicitly[ColumnHelper[T]].lookup(wo, p)
    val cellParser = implicitly[CellParser[P]]
    val idx = row.getIndex(columnName)
    if (idx >= 0) for (w <- row(idx); z <- cellParser.parse(CellValue(w)).recoverWith {
      case NonFatal(e) => Failure(InvalidParseException(s"Problem parsing '$w' as ${implicitly[ClassTag[T]].runtimeClass} from $columnName at index $idx of $row", e))
    }) yield z
    else cellParser.parse(Some(columnName), row, columns).recoverWith {
      case _: UnsupportedOperationException => Failure(ParserException(s"unable to find value for column ${columnName.toUpperCase} in $columns"))
    }
  }
}

/**
  * This companion object comprises CellParser[T] objects which represent conversions that are fixed,
  * i.e. they don't depend on some other parameter such as the formatter in DateTime conversions.
  */
object CellParsers

case class ParsersException(w: String) extends Exception(w)

/**
  * This class is used for the situation where a column in a table actually contains a set of
  * attributes, typically separated by "," and possibly bracketed by "{}".
  * CONSIDER allowing "|" as a separator (as described previously in the documentation here).
  *
  * @param xs the attribute values.
  */
case class AttributeSet(xs: StringList)

object AttributeSet {
  def apply(w: String): AttributeSet = parse(w).get

  def parse(w: String): Try[AttributeSet] = Parseable.split(w).map(apply)
}

