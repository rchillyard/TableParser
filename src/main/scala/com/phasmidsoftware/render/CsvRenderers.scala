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
   * @param construct a function P => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam P1 the type of the (single) field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer1[P1: CsvRenderer, T <: Product : ClassTag](construct: P1 => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T]() {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
    )
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
  def rendererGenerator1[P1: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: P1 => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {

    def elements(t: T): Strings = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
    )

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator1(construct).asInstanceOf[CsvProductGenerator[T]].toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
   *
   * @param construct a function (P1,P2) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer2[P1: CsvRenderer, P2: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T]() {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p2 = t.productElement(1).asInstanceOf[P2]
      val constructFirst: P1 => T = construct(_, p2)
      val sequenceFirst = renderer1(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P2]].render(p2)
    }
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
  def rendererGenerator2[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {

    def elements(t: T): Strings = renderer2(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator2(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
   *
   * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer3[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T]() {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p3 = t.productElement(2).asInstanceOf[P3]
      val constructFirst: (P1, P2) => T = construct(_, _, p3)
      val sequenceFirst = renderer2(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P3]].render(p3)
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
  def rendererGenerator3[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer3(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator3(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 4-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer4[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T]() {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p4 = t.productElement(3).asInstanceOf[P4]
      val constructFirst: (P1, P2, P3) => T = construct(_, _, _, p4)
      val sequenceFirst = renderer3(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P4]].render(p4)
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
  def rendererGenerator4[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer4(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator4(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 5-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer5[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, T <: Product : ClassTag](construct: (P1, P2, P3, P4, P5) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T]() {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p5 = t.productElement(4).asInstanceOf[P5]
      val constructFirst: (P1, P2, P3, P4) => T = construct(_, _, _, _, p5)
      val sequenceFirst = renderer4(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P5]].render(p5)
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
  def rendererGenerator5[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer5(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator5(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 6-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer6[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p6 = t.productElement(5).asInstanceOf[P6]
      val constructFirst: (P1, P2, P3, P4, P5) => T = construct(_, _, _, _, _, p6)
      val sequenceFirst = renderer5(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P6]].render(p6)
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
  def rendererGenerator6[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer6(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator6(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 7-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def renderer7[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p7 = t.productElement(6).asInstanceOf[P7]
      val constructFirst: (P1, P2, P3, P4, P5, P6) => T = construct(_, _, _, _, _, _, p7)
      val sequenceFirst = renderer6(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P7]].render(p7)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 7-ary Product and which is based on the given "construct" function.
   *
   * TEST
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
  def rendererGenerator7[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer7(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator7(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 8-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def renderer8[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p8 = t.productElement(7).asInstanceOf[P8]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7) => T = construct(_, _, _, _, _, _, _, p8)
      val sequenceFirst = renderer7(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P8]].render(p8)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 8-ary Product and which is based on the given "construct" function.
   *
   * TEST
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
  def rendererGenerator8[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer8(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator8(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 9-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def renderer9[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p9 = t.productElement(8).asInstanceOf[P9]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8) => T = construct(_, _, _, _, _, _, _, _, p9)
      val sequenceFirst = renderer8(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P9]].render(p9)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 9-ary Product and which is based on the given "construct" function.
   *
   * TEST
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
  def rendererGenerator9[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer9(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator9(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 10-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def renderer10[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p10 = t.productElement(9).asInstanceOf[P10]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T = construct(_, _, _, _, _, _, _, _, _, p10)
      val sequenceFirst = renderer9(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P10]].render(p10)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 10-ary Product and which is based on the given "construct" function.
   *
   * TEST
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
  def rendererGenerator10[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {
    def elements(t: T): Strings = renderer10(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator10(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 11-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def renderer11[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, P11: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p11 = t.productElement(10).asInstanceOf[P11]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T = construct(_, _, _, _, _, _, _, _, _, _, p11)
      val sequenceFirst = renderer10(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P11]].render(p11)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 11-ary Product and which is based on the given "construct" function.
   *
   * TEST
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def rendererGenerator11[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, P11: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T]() {
    def elements(t: T): Strings = renderer11(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator11(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 12-ary Product and which is based on the given "construct" function.
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @param ca        the (implicit) CsvAttributes.
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
  def renderer12[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, P11: CsvRenderer, P12: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    def elements(t: T): Strings = {
      val p12 = t.productElement(11).asInstanceOf[P12]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T = construct(_, _, _, _, _, _, _, _, _, _, _, p12)
      val sequenceFirst = renderer11(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P12]].render(p12)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 12-ary Product and which is based on the given "construct" function.
   *
   * TEST
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
  def rendererGenerator12[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, P11: CsvRenderer : CsvGenerator, P12: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T)(implicit csvAttributes: CsvAttributes): CsvRenderer[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer12(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator12(construct).toColumnNames(po, no)
  }
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
