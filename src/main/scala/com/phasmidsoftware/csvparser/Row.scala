package com.phasmidsoftware.csvparser

import scala.reflect.ClassTag
import scala.util.Try

trait Row extends Product {

  def header: Seq[String]

  def maybeIndex(w: String): Option[Int] = header.indexOf(w) match {
    case -1 => None
    case x => Some(x)
  }

  def asMap: Map[String, Any] = (header zip productIterator.toSeq).toMap
}

abstract class TupleRow(classTags: Seq[ClassTag[_]], columns: Seq[String]) extends Row {
  def header: Seq[String] = columns

  def getCell[T: ClassTag](x: Int): Option[T] = if (classTags(x).runtimeClass.isAssignableFrom(implicitly[ClassTag[T]].runtimeClass)) Some(productElement(x).asInstanceOf[T]) else None

  def getCell[T: ClassTag](w: String): Option[T] = for (i <- maybeIndex(w); c <- getCell(i)) yield c
}

case class SeqRow[T](elements: Seq[T])(val header: Seq[String] = Nil) extends Row {
  self =>

  override def toString: String = productPrefix + ": " + asMap

  override def productElement(n: Int): Any = elements(n)

  override def productArity: Int = elements.size

  def getCell(x: Int): Option[T] = Try(productElement(x).asInstanceOf[T]).toOption

  def getCell(w: String): Option[T] = for (i <- maybeIndex(w); c <- getCell(i)) yield c

  def map[U](f: T => U): SeqRow[U] = SeqRow(elements map f)(header)

  def flatMap[U](f: T => SeqRow[U]): SeqRow[U] = (elements map f).foldLeft(SeqRow[U](Nil)(Nil))(_ ++ _)

  def ++[U >: T](row: SeqRow[U]): SeqRow[U] = SeqRow[U](self.elements ++ row.elements)(self.header ++ row.header)
}

object SeqRow {

  def apply[T](row: Row)(f: Any => T): SeqRow[T] = apply[T](row.productIterator.toSeq map f)(row.header)

  def create[T](elements: Seq[T], header: String*): SeqRow[T] = apply(elements)(header)
}

case class Row1[T: ClassTag](t: T)(columns: Seq[String] = Nil) extends TupleRow(Seq(implicitly[ClassTag[T]].asInstanceOf[ClassTag[_]]), columns)

object Row1 {
  def apply[T: ClassTag](row: Row)(f: Any => T): Row1[T] = apply[T](f(row.productElement(0)))(row.header)

  def create[T: ClassTag](t: T, header: String*): Row1[T] = apply(t)(header)
}

case class Row2[T0: ClassTag, T1: ClassTag](t0: T0, t1: T1)(columns: Seq[String] = Nil) extends TupleRow(Seq(implicitly[ClassTag[T0]].asInstanceOf[ClassTag[_]], implicitly[ClassTag[T1]].asInstanceOf[ClassTag[_]]), columns) {
  def asTuple: (T0, T1) = (t0, t1)
}

object Row2 {
  def apply[T0: ClassTag, T1: ClassTag](row: Row)(f0: Any => T0, f1: Any => T1): Row2[T0, T1] = apply[T0, T1](f0(row.productElement(0)), f1(row.productElement(1)))(row.header)

  def create[T0: ClassTag, T1: ClassTag](t0: T0, t1: T1, header: String*): Row2[T0, T1] = apply(t0, t1)(header)
}

case class Row3[T0: ClassTag, T1: ClassTag, T2: ClassTag](t0: T0, t1: T1, t2: T2)(columns: Seq[String] = Nil) extends TupleRow(Seq(implicitly[ClassTag[T0]].asInstanceOf[ClassTag[_]], implicitly[ClassTag[T1]].asInstanceOf[ClassTag[_]], implicitly[ClassTag[T2]].asInstanceOf[ClassTag[_]]), columns) {
  def asTuple: (T0, T1, T2) = (t0, t1, t2)
}

object Row3 {
  def apply[T0: ClassTag, T1: ClassTag, T2: ClassTag](row: Row)(f0: Any => T0, f1: Any => T1, f2: Any => T2): Row3[T0, T1, T2] = apply[T0, T1, T2](f0(row.productElement(0)), f1(row.productElement(1)), f2(row.productElement(2)))(row.header)

  def create[T0: ClassTag, T1: ClassTag, T2: ClassTag](t0: T0, t1: T1, t2: T2, header: String*): Row3[T0, T1, T2] = apply(t0, t1, t2)(header)
}

case class RowException(w: String) extends Exception(w)