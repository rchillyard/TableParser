package com.phasmidsoftware.format

import com.phasmidsoftware.tableparser._

import scala.reflect.ClassTag

/**
  * Trait to define the various formats for reading case classes from table rows.
  */
trait Formats {

  def cellReader1[P1: CellParser, T <: Product : ClassTag](construct: P1 => T): CellParser[T] = {
    val Array(p1) = Formats.extractFieldNames(implicitly[ClassTag[T]])
    new MultiCellParser[T] {
      def read(row: Row, columns: Seq[String]): T = construct(implicitly[CellParser[P1]].read(CellValue(row(p1))))
    }
  }

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
}

object Formats {

  implicit object IntCellParser$ extends SingleCellParser[Int] {
    def convertString(w: String): Int = w.toInt
  }

  implicit object LongCellParser$ extends SingleCellParser[Long] {
    override def convertString(w: String): Long = w.toLong
  }

  implicit object StringCellParser$ extends SingleCellParser[String] {
    override def convertString(w: String): String = w
  }

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