package com.phasmidsoftware.parse

import com.phasmidsoftware.table._

import scala.reflect.ClassTag

/**
  * Trait to define the various formats for reading case classes from table rows.
  *
  * NOTE In each of these cellReader methods, the CellParser has a read method which ignores the columns.
  *
  * CONSIDER renaming this trait.
  *
  * CONSIDER renaming the methods.
  */
trait Formats {

  /**
    * Method to return a CellParser[Seq[P].
    *
    * @tparam P the underlying type of the result
    * @return a MultiCellParser[Seq[P]
    */
  def cellReaderSeq[P: CellParser]: CellParser[Seq[P]] = {
    new MultiCellParser[Seq[P]] {
      def read(row: Row, columns: Seq[String]): Seq[P] = for (w <- row.ws) yield implicitly[CellParser[P]].read(CellValue(w))
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
  def cellReader[P: CellParser, T: ClassTag](construct: P => T): CellParser[T] = {
    new SingleCellParser[T] {
      def convertString(w: String): T = construct(implicitly[CellParser[P]].read(CellValue(w)))
    }
  }

  /**
    * Method to return a CellParser[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * @param construct a function P => T, usually the apply method of a case class.
    * @tparam P1 the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a MultiCellParser which converts a String from a Row into the field type P and thence into a T
    */
  def cellReader1[P1: CellParser, T <: Product : ClassTag](construct: P1 => T): CellParser[T] = {
    val Array(p1) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      def read(row: Row, columns: Seq[String]): T = construct(implicitly[CellParser[P1]].read(CellValue(row(p1))))
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
  def cellReader2[P1: CellParser, P2: CellParser, T <: Product : ClassTag](construct: (P1, P2) => T): CellParser[T] = {
    val Array(p1, p2) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      override def read(row: Row, columns: Seq[String]): T = {
        val p1V = implicitly[CellParser[P1]].read(CellValue(row(p1)))
        val p2V = implicitly[CellParser[P2]].read(CellValue(row(p2)))
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
  def cellReader3[P1: CellParser, P2: CellParser, P3: CellParser, T <: Product : ClassTag](construct: (P1, P2, P3) => T): CellParser[T] = {
    val Array(p1, p2, p3) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      override def read(row: Row, columns: Seq[String]): T = {
        val p1V = implicitly[CellParser[P1]].read(CellValue(row(p1)))
        val p2V = implicitly[CellParser[P2]].read(CellValue(row(p2)))
        val p3V = implicitly[CellParser[P3]].read(CellValue(row(p3)))
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
  def cellReader4[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T): CellParser[T] = {
    val Array(p1, p2, p3, p4) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      override def read(row: Row, columns: Seq[String]): T = {
        val p1V = implicitly[CellParser[P1]].read(CellValue(row(p1)))
        val p2V = implicitly[CellParser[P2]].read(CellValue(row(p2)))
        val p3V = implicitly[CellParser[P3]].read(CellValue(row(p3)))
        val p4V = implicitly[CellParser[P4]].read(CellValue(row(p4)))
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
  def cellReader5[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5) => T): CellParser[T] = {
    val Array(p1, p2, p3, p4, p5) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      override def read(row: Row, columns: Seq[String]): T = {
        val p1V = implicitly[CellParser[P1]].read(CellValue(row(p1)))
        val p2V = implicitly[CellParser[P2]].read(CellValue(row(p2)))
        val p3V = implicitly[CellParser[P3]].read(CellValue(row(p3)))
        val p4V = implicitly[CellParser[P4]].read(CellValue(row(p4)))
        val p5V = implicitly[CellParser[P5]].read(CellValue(row(p5)))
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
  def cellReader6[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6) => T): CellParser[T] = {
    val Array(p1, p2, p3, p4, p5, p6) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      override def read(row: Row, columns: Seq[String]): T = {
        val p1V = implicitly[CellParser[P1]].read(CellValue(row(p1)))
        val p2V = implicitly[CellParser[P2]].read(CellValue(row(p2)))
        val p3V = implicitly[CellParser[P3]].read(CellValue(row(p3)))
        val p4V = implicitly[CellParser[P4]].read(CellValue(row(p4)))
        val p5V = implicitly[CellParser[P5]].read(CellValue(row(p5)))
        val p6V = implicitly[CellParser[P6]].read(CellValue(row(p6)))
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
  def cellReader7[P1: CellParser, P2: CellParser, P3: CellParser, P4: CellParser, P5: CellParser, P6: CellParser, P7: CellParser, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7) => T): CellParser[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      override def read(row: Row, columns: Seq[String]): T = {
        val p1V = implicitly[CellParser[P1]].read(CellValue(row(p1)))
        val p2V = implicitly[CellParser[P2]].read(CellValue(row(p2)))
        val p3V = implicitly[CellParser[P3]].read(CellValue(row(p3)))
        val p4V = implicitly[CellParser[P4]].read(CellValue(row(p4)))
        val p5V = implicitly[CellParser[P5]].read(CellValue(row(p5)))
        val p6V = implicitly[CellParser[P6]].read(CellValue(row(p6)))
        val p7V = implicitly[CellParser[P7]].read(CellValue(row(p7)))
        construct(p1V, p2V, p3V, p4V, p5V, p6V, p7V)
      }
    }
  }
}

/**
  * This companion object comprises CellParser[T] objects which represent conversions that are fixed,
  * i.e. they don't depend on some other parameter such as the formatter in DateTime conversions.
  */
object Formats {

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

case class FormatsException(w: String) extends Exception(w)