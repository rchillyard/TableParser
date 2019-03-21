package com.phasmidsoftware.format

import java.lang.reflect.Modifier

import scala.util.control.NonFatal

case class Row(ws: Seq[String], hdr: Seq[String]) extends (String => String) {
  /**
    * Method to yield the value for a given column name
    *
    * @param w the column name
    * @return the value as a String
    * @throws IndexOutOfBoundsException if w is not contained in hdr or hdr is longer than ws.
    */
  override def apply(w: String): String = ws(hdr.indexOf(w))
}


trait Formats {

  def cellReader1[P1: CellReader, T <: Product : ClassManifest](construct: P1 => T): CellReader[T] = {
    val Array(p1) = Formats.extractFieldNames(classManifest[T])
    cellReader(construct, p1)
  }

  def cellReader[P1: CellReader, T <: Product](construct: P1 => T, fieldName1: String): CellReader[T] = new RowReader[T] {

    //    override def read(value: Convertible): T = {
    //      val p1V = implicitly[CellReader[P1]].read(value)
    //      construct(p1V)
    //    }
    override def read(row: Row, columns: Seq[String]): T = {
      val p1V = implicitly[CellReader[P1]].read(CellValue(row(fieldName1)))
      construct(p1V)

    }
  }

  def cellReader2[P1: CellReader, P2: CellReader, T <: Product : ClassManifest](construct: (P1, P2) => T): CellReader[T] = {
    val Array(p1, p2) = Formats.extractFieldNames(classManifest[T])
    cellReader(construct, p1, p2)
  }

  def cellReader[P1: CellReader, P2: CellReader, T <: Product](construct: (P1, P2) => T, fieldName1: String, fieldName2: String): CellReader[T] = new RowReader[T] {
    //    override def read(row: Row, column: String): T = {
    //      val p1V = implicitly[CellReader[P1]].read(CellValue(row(fieldName1)))
    //      val p2V = implicitly[CellReader[P2]].read(CellValue(row(fieldName2)))
    //      construct(p1V, p2V)
    //    }
    //
    //    override def read(value: Convertible): T = {
    //      val p1V = implicitly[CellReader[P1]].read(value)
    //      val p2V = implicitly[CellReader[P2]].read(value)
    //      construct(p1V,p2V)
    //    }
    override def read(row: Row, columns: Seq[String]): T = {
      val p1V = implicitly[CellReader[P1]].read(CellValue(row(fieldName1)))
      val p2V = implicitly[CellReader[P2]].read(CellValue(row(fieldName2)))
      construct(p1V, p2V)
    }
  }
}

trait CellReader[T] {
  // Need to define this better so that we don't have any non-implemented methods.
  def convertString(w: String): T

  def read(value: Convertible): T = value match {
    case CellValue(w) => convertString(w)
    case RowValues(row, columns) => read(row, columns)
    case _ => throw FormatException(s"ValueCellReader: Cannot convert value $value of type ${value.getClass}")
  }

  def read(row: Row, columns: Seq[String]): T
}

trait SingleCellReader[T] extends CellReader[T] {
  def read(row: Row, columns: Seq[String]): T = ???
}

trait RowReader[T] extends CellReader[T] {
  def convertString(w: String): T = ???
}

//trait RowReader[T] {
//
//  def read(value: Row, column: String)(implicit cr: CellReader[T]): T = cr.read(CellValue(value.apply(column)))
//}

sealed abstract class Convertible {
  def convertTo[T: CellReader]: T = cellReader.read(this)
}

case class CellValue(w: String) extends Convertible

//case class CellValues(ws: Seq[String]) extends Convertible

case class RowValues(row: Row, ws: Seq[String]) extends Convertible

object Formats {

  implicit object IntCellReader extends SingleCellReader[Int] {
    def convertString(w: String): Int = w.toInt
  }

  implicit object LongCellReader extends SingleCellReader[Long] {
    override def convertString(w: String): Long = w.toLong
  }

  implicit object StringCellReader extends SingleCellReader[String] {
    override def convertString(w: String): String = w
  }

  def extractFieldNames(classManifest: ClassManifest[_]): Array[String] = {
    val clazz = classManifest.erasure
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

case class FormatException(w: String) extends Exception(w)