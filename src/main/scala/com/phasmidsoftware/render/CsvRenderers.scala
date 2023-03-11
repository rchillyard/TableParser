/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.parse.Strings
import com.phasmidsoftware.table._
import java.net.URL
import scala.reflect.ClassTag

/**
 * Trait to define various renderers for rendering instance of case classes (with their various parameters),
 * containers (Seq and Option), etc. to CSV output.
 *
 * CONSIDER a mechanism to ensure that objects involving case classes are presented in the same order as specified by the header.
 */
trait CsvRenderers {

  /**
   * Method to return a CsvRenderer[RawRow].
   *
   * @param ca the (implicit) CsvAttributes.
   * @return a CsvRenderer[RawRow].
   */
  def rawRowRenderer(implicit ca: CsvAttributes): CsvRenderer[RawRow] = new CsvRenderer[RawRow] {
    implicit val z: CsvRenderer[String] = CsvRenderers.CsvRendererString

    def render(t: RawRow, attrs: Map[String, String]): String = sequenceRenderer[String].render(t.ws)

    val csvAttributes: CsvAttributes = ca
  }

  /**
   * Method to return a CsvRenderer[ Seq[T] ].
   *
   * @param ca the (implicit) CsvAttributes.
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
   * @param ca the (implicit) CsvAttributes.
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[ Option[T] ].
   */
  def optionRenderer[T: CsvRenderer](implicit ca: CsvAttributes): CsvRenderer[Option[T]] = new CsvRenderer[Option[T]] {
    val csvAttributes: CsvAttributes = ca

    def render(to: Option[T], attrs: Map[String, String]): String = (to map (t => implicitly[CsvRenderer[T]].render(t))).getOrElse("")
  }

  /**
   * Method to return a CsvRenderer[T] which does not output a T at all, only a number of delimiters according to the value of alignment.
   *
   * @param alignment (defaults to 1): one more than the number of delimiters to output.
   *                  If you are skipping a Product (such as a case class instance), then you should carefully count up how many (nested) elements to skip.
   *                  So, for example, if you are skipping a Product with three members, you would set alignment = 3, even though you only want to output 2 delimiters.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam T the type of the parameter to the render method.
   * @return a CsvRenderer[T].
   */
  def skipRenderer[T](alignment: Int = 1)(implicit ca: CsvAttributes): CsvRenderer[T] = new CsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def render(t: T, attrs: Map[String, String]): String = ca.delimiter * (alignment - 1)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
   *
   * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
   *
   * @param construct     a function P => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the (single) field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer1[P1: CsvRenderer, T <: Product : ClassTag](construct: P1 => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
    )
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
   *
   * @param construct     a function (P1,P2) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer2[P1: CsvRenderer, P2: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {
    def elements(t: T): Strings = {
      val p2 = t.productElement(1).asInstanceOf[P2]
      val constructFirst: P1 => T = construct(_, p2)
      val sequenceFirst = renderer1(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P2]].render(p2)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
   *
   * @param construct     a function (P1,P2,P3) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer3[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p3 = t.productElement(2).asInstanceOf[P3]
      val constructFirst: (P1, P2) => T = construct(_, _, p3)
      val sequenceFirst = renderer2(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P3]].render(p3)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 4-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer4[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p4 = t.productElement(3).asInstanceOf[P4]
      val constructFirst: (P1, P2, P3) => T = construct(_, _, _, p4)
      val sequenceFirst = renderer3(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P4]].render(p4)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 5-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer5[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p5 = t.productElement(4).asInstanceOf[P5]
      val constructFirst: (P1, P2, P3, P4) => T = construct(_, _, _, _, p5)
      val sequenceFirst = renderer4(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P5]].render(p5)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 6-ary Product and which is based on the given "construct" function.
   *
   * TEST
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer6[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p6 = t.productElement(5).asInstanceOf[P6]
      val constructFirst: (P1, P2, P3, P4, P5) => T = construct(_, _, _, _, _, p6)
      val sequenceFirst = renderer5(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P6]].render(p6)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 7-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer7[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p7 = t.productElement(6).asInstanceOf[P7]
      val constructFirst: (P1, P2, P3, P4, P5, P6) => T = construct(_, _, _, _, _, _, p7)
      val sequenceFirst = renderer6(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P7]].render(p7)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 8-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam P8 the type of the eighth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer8[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p8 = t.productElement(7).asInstanceOf[P8]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7) => T = construct(_, _, _, _, _, _, _, p8)
      val sequenceFirst = renderer7(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P8]].render(p8)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 9-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
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
   * @return a CsvRenderer[T].
   */
  def renderer9[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p9 = t.productElement(8).asInstanceOf[P9]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8) => T = construct(_, _, _, _, _, _, _, _, p9)
      val sequenceFirst = renderer8(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P9]].render(p9)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 10-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
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
   * @return a CsvRenderer[T].
   */
  def renderer10[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p10 = t.productElement(9).asInstanceOf[P10]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T = construct(_, _, _, _, _, _, _, _, _, p10)
      val sequenceFirst = renderer9(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P10]].render(p10)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 11-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
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
   * @return a CsvRenderer[T].
   */
  def renderer11[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, P11: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p11 = t.productElement(10).asInstanceOf[P11]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T = construct(_, _, _, _, _, _, _, _, _, _, p11)
      val sequenceFirst = renderer10(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P11]].render(p11)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 12-ary Product and which is based on the given "construct" function.
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
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
   * @return a CsvRenderer[T].
   */
  def renderer12[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, P11: CsvRenderer, P12: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {

    def elements(t: T): Strings = {
      val p12 = t.productElement(11).asInstanceOf[P12]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T = construct(_, _, _, _, _, _, _, _, _, _, _, p12)
      val sequenceFirst = renderer11(constructFirst).asInstanceOf[ProductCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P12]].render(p12)
    }
  }
}

abstract class ProductCsvRenderer[T <: Product : ClassTag](implicit c: CsvAttributes) extends CsvRenderer[T] {
  def elements(t: T): Strings

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

  implicit object CsvRendererURL extends StandardCsvRenderer[URL]
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
   * Method to return a CsvGenerator[T] which does not output a column header for at all.
   *
   * @tparam T the type of the column objects.
   * @return a CsvGenerator[T].
   */
  def skipGenerator[T](implicit ca: CsvAttributes): CsvGenerator[T] = new CsvProductGenerator[T] {
    val csvAttributes: CsvAttributes = ca

    override def toColumnName(po: Option[String], name: String): String = ""

    // TEST (not actually used).
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
  def generator2[P1: CsvGenerator, P2: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator3[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator4[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator5[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator6[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator7[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator8[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator9[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator10[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator11[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  def generator12[P1: CsvGenerator, P2: CsvGenerator, P3: CsvGenerator, P4: CsvGenerator, P5: CsvGenerator, P6: CsvGenerator, P7: CsvGenerator, P8: CsvGenerator, P9: CsvGenerator, P10: CsvGenerator, P11: CsvGenerator, P12: CsvGenerator, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T)(implicit c: CsvAttributes): CsvProductGenerator[T] = new BaseCsvGenerator[T]() with CsvProductGenerator[T] {
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
  implicit object CsvGeneratorBoolean extends BaseCsvGenerator[Boolean]

  implicit object CsvGeneratorInt extends BaseCsvGenerator[Int]

  implicit object CsvGeneratorLong extends BaseCsvGenerator[Long]

  implicit object CsvGeneratorDouble extends BaseCsvGenerator[Double]

  implicit object CsvGeneratorString extends BaseCsvGenerator[String]

  implicit object CsvGeneratorURL extends BaseCsvGenerator[URL]
}
