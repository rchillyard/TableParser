/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{BaseCsvGenerator, CsvAttributes, CsvGenerator, CsvProductGenerator}

import scala.reflect.ClassTag

/**
  * Trait to define various renderers for rendering instance of case classes (with their various parameters),
  * containers (Seq and Option), etc. to CSV output.
  *
  * CONSIDER a mechanism to ensure that objects involving case classes are presented in the same order as specified by the header.
  */
trait CsvRenderers {

  /**
    * Method to return a CsvRenderer[ Seq[T] ].
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a CsvRenderer[ Seq[T] ]
    */
  def sequenceRenderer[T: CsvRenderer](implicit ca: CsvAttributes): CsvRenderer[Seq[T]] = new CsvRenderer[Seq[T]] {

    def render(ts: Seq[T], attrs: Map[String, String]): String = (ts map { t: T => implicitly[CsvRenderer[T]].render(t) }).mkString(csvAttributes.delimiter)

    val csvAttributes: CsvAttributes = ca
  }

  /**
    * Method to return a CsvRenderer[ Option[T] ].
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a CsvRenderer[ Option[T] ].
    */
  def optionRenderer[T: CsvRenderer](implicit ca: CsvAttributes): CsvRenderer[Option[T]] = new CsvRenderer[Option[T]] {
    val csvAttributes: CsvAttributes = ca

    def render(to: Option[T], attrs: Map[String, String]): String = (to map (t => implicitly[CsvRenderer[T]].render(t))).getOrElse("")
  }

  /**
    * Method to return a CsvRenderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
    *
    * @param construct a function P => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a HierarchicalRenderer[T].
    */
  def renderer1[P1: CsvRenderer, T <: Product : ClassTag](construct: P1 => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    protected def elements(t: T): Seq[String] = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
    )
  }

  /**
    * Method to return a CsvRenderer[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
    *
    * @param construct a function (P1,P2) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a CsvRenderer[T].
    */
  def renderer2[P1: CsvRenderer, P2: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    protected def elements(t: T): Seq[String] = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
      , implicitly[CsvRenderer[P2]].render(t.productElement(1).asInstanceOf[P2])
    )
  }

  /**
    * Method to return a CsvRenderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
    *
    * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a CsvRenderer[T].
    */
  def renderer3[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    protected def elements(t: T): Seq[String] = {
      Seq(
        implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
        , implicitly[CsvRenderer[P2]].render(t.productElement(1).asInstanceOf[P2])
        , implicitly[CsvRenderer[P3]].render(t.productElement(2).asInstanceOf[P3])
      )
    }
  }

  /**
    * Method to return a CsvRenderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3,P4) into a T.
    *
    * @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the third field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a CsvRenderer[T].
    */
  def renderer4[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    protected def elements(t: T): Seq[String] = {
      Seq(
        implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
        , implicitly[CsvRenderer[P2]].render(t.productElement(1).asInstanceOf[P2])
        , implicitly[CsvRenderer[P3]].render(t.productElement(2).asInstanceOf[P3])
        , implicitly[CsvRenderer[P4]].render(t.productElement(3).asInstanceOf[P4])
      )
    }
  }
}

abstract class ProductCsvRenderer[T <: Product : ClassTag](implicit c: CsvAttributes) extends CsvRenderer[T] {
  protected def elements(t: T): Seq[String]

  val csvAttributes: CsvAttributes = c

  def render(t: T, attrs: Map[String, String]): String = elements(t) mkString csvAttributes.delimiter
}

object CsvRenderers {
  abstract class StandardCsvRenderer[T] extends CsvRenderer[T] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(t: T, attrs: Map[String, String]): String = t.toString
  }

  implicit object CsvRendererBoolean extends StandardCsvRenderer[Boolean]

  implicit object CsvRendererInt extends StandardCsvRenderer[Int]

  implicit object CsvRendererLong extends StandardCsvRenderer[Long]

  implicit object CsvRendererDouble extends StandardCsvRenderer[Double]

  implicit object CsvRendererString extends StandardCsvRenderer[String]
}

trait CsvGenerators {

  /**
    * Method to return a CsvGenerator[ Seq[T] ].
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a CsvGenerator[ Seq[T] ]
    */
  def sequenceGenerator[T](implicit ca: CsvAttributes): CsvGenerator[Seq[T]] = new BaseCsvGenerator[Seq[T]]

  /**
    * Method to return a CsvGenerator[ Option[T] ].
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a CsvGenerator[ Option[T] ].
    */
  def optionGenerator[T](implicit ca: CsvAttributes): CsvGenerator[Option[T]] = new BaseCsvGenerator[Option[T]]

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
  def generator1[P1: CsvGenerator, T <: Product : ClassTag](construct: P1 => T)(implicit c: CsvAttributes): CsvGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generators2[P1: CsvGenerator, P2: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generators3[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generators4[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
}

object CsvGenerators {
  implicit object CsvGeneratorBoolean extends BaseCsvGenerator[Boolean]

  implicit object CsvGeneratorInt extends BaseCsvGenerator[Int]

  implicit object CsvGeneratorLong extends BaseCsvGenerator[Long]

  implicit object CsvGeneratorDouble extends BaseCsvGenerator[Double]

  implicit object CsvGeneratorString extends BaseCsvGenerator[String]
}
