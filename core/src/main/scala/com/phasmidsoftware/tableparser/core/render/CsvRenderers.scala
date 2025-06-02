/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.Strings
import com.phasmidsoftware.tableparser.core.table._
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

    /**
     * Renders a sequence of strings based on the provided `RawRow` and attribute map.
     *
     * @param t     The `RawRow` instance containing the sequence of strings to be rendered.
     * @param attrs A map of attributes that can be used to influence the rendering process.
     *              Keys are attribute names, and values are their corresponding values.
     * @return A `String` representation of the rendered sequence.
     */
    def render(t: RawRow, attrs: Map[String, String]): String =
      sequenceRenderer[String].render(t.ws)

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

    /**
     * Renders a sequence of objects into a CSV-formatted string using provided attributes.
     *
     * @param ts    The sequence of objects of type `T` to be rendered into CSV format.
     * @param attrs A map of attribute key-value pairs representing options or settings for the CSV rendering process.
     * @return A string representing the sequence `ts` in CSV format, concatenated using the delimiter defined in `csvAttributes`.
     */
    def render(ts: Seq[T], attrs: Map[String, String]): String =
      (ts map { t: T => implicitly[CsvRenderer[T]].render(t) }).mkString(csvAttributes.delimiter)

    val csvAttributes: CsvAttributes = ca
  }

  /**
   * Method to return a CsvRenderer[ Option[T] ].
   *
   * @param ca the (implicit) CsvAttributes.
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[ Option[T] ].
   */
  def optionRenderer[T: CsvRenderer](defaultString: String = "")(implicit ca: CsvAttributes): CsvRenderer[Option[T]] = new CsvRenderer[Option[T]] {
    val csvAttributes: CsvAttributes = ca

    /**
     * Renders an optional value of type `T` into a string representation using a `CsvRenderer`
     * and applies default string rendering if the value is absent.
     *
     * @param to    An optional value of type `T` that needs to be rendered. If present, it will
     *              be rendered using the implicitly provided `CsvRenderer[T]`.
     * @param attrs A map of string attributes that can optionally be used during
     *              the rendering process. Currently, these are not utilized in the logic.
     * @return A string representation of the provided value if present, rendered using
     *         the `CsvRenderer[T]`. Otherwise, returns a default string.
     */
    def render(to: Option[T], attrs: Map[String, String]): String =
      (to map (t => implicitly[CsvRenderer[T]].render(t))).getOrElse(defaultString)
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

    /**
     * Renders the given input along with specified attributes into a formatted string.
     *
     * @param t     The input of type `T` to be rendered.
     * @param attrs A map of attribute key-value pairs that provide additional rendering parameters.
     * @return A formatted string representation of the input `t` based on the attributes and internal logic.
     */
    def render(t: T, attrs: Map[String, String]): String =
      ca.delimiter * (alignment - 1)
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

    /**
     * Transforms a given product element of type `T` into its string representation.
     * This method utilizes an implicit `CsvRenderer` to render the first product element.
     *
     * @param t The input of type `T` whose first product element will be rendered.
     * @return A sequence of strings containing the rendered first product element.
     */
    def elements(t: T): Strings = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
    )
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
   *
   * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
   *
   * TESTME
   *
   * @param construct     a function P => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the (single) field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvProduct[T].
   */
  def rendererGenerator1[P1: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: P1 => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {

    /**
     * Processes an input of type `T` and converts its first product element to a string representation
     * using the implicitly available `CsvRenderer` for the specific type of the element.
     *
     * @param t The input of type `T` whose first product element will be rendered as a string.
     * @return A sequence of strings containing the rendered first product element of the input `t`.
     */
    def elements(t: T): Strings = Seq(
      implicitly[CsvRenderer[P1]].render(t.productElement(0).asInstanceOf[P1])
    )

    /**
     * Converts optional prefix and suffix into a string of column names based on the CSV product generator.
     *
     * @param po An optional string representing the prefix for the column names. If `None`, no prefix is applied.
     * @param no An optional string representing the suffix for the column names. If `None`, no suffix is applied.
     * @return A string representing the column names with the applied prefix and/or suffix.
     */
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

    /**
     * Generates a sequence of strings from the provided input of type `T`.
     * The method extracts specific elements of the input, processes them,
     * and generates a CSV representation.
     *
     * @param t the input of type `T` from which the elements are extracted and processed.
     * @return a sequence of strings representing the processed elements in CSV format.
     */
    def elements(t: T): Strings = {
      val p2 = t.productElement(1).asInstanceOf[P2]
      val constructFirst: P1 => T = construct(_, p2)
      val sequenceFirst = renderer1(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P2]].render(p2)
    }
  }

  /**
   * Generates a CSV renderer and generator for a product type with two parameters.
   * This method creates an instance of `ProductCsvRenderer` for handling CSV functionality,
   * such as rendering elements and generating column names based on the provided
   * constructor function and implicit attributes.
   *
   * @param construct     A function that constructs a product type `T` from two parameters `P1` and `P2`.
   *                      The types `P1` and `P2` must have both `CsvRenderer` and `CsvGenerator` type classes available.
   * @param csvAttributes Implicit CSV attributes used for rendering and generating CSV.
   * @tparam P1 The type of the first parameter in the product.
   * @tparam P2 The type of the second parameter in the product.
   * @tparam T  The type of the product created.
   * @return A `CsvProduct[T]` which is a CSV renderer and generator specialized for the given product type `T`.
   */
  def rendererGenerator2[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {

    def elements(t: T): Strings =
      renderer2(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

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

    /**
     * Constructs a sequence of string representations for the elements of a given value of type `T`.
     *
     * @param t The value of type `T` whose elements are to be rendered as strings.
     * @return A sequence of strings representing the elements of the given value `t`.
     *         The sequence includes the rendered representation of the third product element (`P3`) and applies
     *         the constructed renderer for the first two product elements (`P1` and `P2`).
     */
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
   * TESTME
   *
   * @param construct     a function (P1,P2,P3) => T, usually the apply method of a case class.
   *                      The sole purpose of this function is for type inference--it is never actually invoked.
   * @param csvAttributes the (implicit) CsvAttributes.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a CsvProduct[T].
   */
  def rendererGenerator3[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
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

    /**
     * Retrieves the CSV elements for the given instance of type `T` by decomposing its components.
     *
     * @param t The instance of type `T` whose elements are to be extracted and rendered.
     * @return A sequence of string representations of the elements of `T`, including the rendered representation of the fourth element.
     */
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator4[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer4(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    // TESTME
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

    /**
     * Processes the elements of a given object of type `T` and returns a sequence of strings
     * representing the extracted and rendered elements.
     *
     * @param t An instance of type `T` from which the elements will be extracted and processed.
     * @return A sequence of strings representing the elements of `t`, including the rendered value
     *         of the fifth product element (P5).
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator5[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
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

    /**
     * Extracts the individual string elements from the provided object of type `T`.
     * This method utilizes a predefined constructor and renderer to process the elements
     * of the object `t`, appending the rendered representation of the sixth product element
     * to the resulting sequence of strings.
     *
     * @param t The input object of type `T` from which the elements will be extracted.
     *          `T` must be a case class or compatible type with product elements.
     * @return A sequence of strings representing the elements of the input object `t`,
     *         with the sixth product element (`P6`) rendered and appended to the result.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator6[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
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

    /**
     * Extracts and renders the elements of the given instance of type `T`. This method specifically handles a
     * 7-tuple-like structure, assuming the 7th element is of type `P7` and needs special rendering.
     * The method uses `construct` and `renderer6` to process the first six elements, and then appends
     * the rendered output of the 7th element.
     *
     * @param t An instance of type `T` containing the elements to be processed and rendered.
     * @return A `Strings` object representing the rendered output of all elements in `t`.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator7[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
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

    /**
     * Extracts and renders all elements of type `T` as strings.
     *
     * The method takes a product-like type `T`, retrieves its eighth element (`P8`),
     * constructs a partial function for earlier elements (P1 to P7), and invokes
     * an appropriate renderer to process the elements of `T`.
     *
     * @param t An instance of the generic type `T` from which the elements are extracted and rendered.
     * @return A sequence of strings representing the rendered elements of the input `T`.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator8[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
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

    /**
     * Extracts and renders the elements of a given instance of type `T` as a collection of strings.
     *
     * @param t An instance of type `T` whose elements need to be processed and rendered.
     * @return A sequence of strings representing the rendered elements of the instance `t`.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator9[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
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

    /**
     * Extracts the elements of a given instance `t` of type `T` and returns them as a sequence of strings.
     * This includes rendering the 10th product element (`P10`) and combining it with a sequence of other rendered elements.
     *
     * @param t The instance of type `T` for which the elements need to be extracted and rendered.
     * @return A sequence of strings representing the rendered elements of the given instance `t`.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator10[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T]() {
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

    /**
     * Generates a sequence of strings representing the elements of the given input `t`.
     *
     * @param t The input of type `T` from which the elements are extracted and processed.
     * @return A sequence of strings containing the rendered elements of `t`.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator11[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, P11: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(implicit ca: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T]() {
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

    /**
     * Generates a sequence of string representations for the elements of the given object `t`,
     * based on the supplied product and renderer logic. This method constructs the sequence
     * of elements using parts of the input and renders them as strings in CSV format.
     *
     * @param t The object of type `T` whose elements need to be extracted and rendered as strings.
     * @return A sequence of strings representing the elements of the given object `t`.
     */
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
   * TESTME
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
   * @return a CsvProduct[T].
   */
  def rendererGenerator12[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, P11: CsvRenderer : CsvGenerator, P12: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer12(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator12(construct).toColumnNames(po, no)
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 13-ary Product and which is based on the given "construct" function.
   *
   * @author IntelliJ AI Assistant
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13) => T, usually the apply method of a case class.
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
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvRenderer[T].
   */
  def renderer13[P1: CsvRenderer, P2: CsvRenderer, P3: CsvRenderer, P4: CsvRenderer, P5: CsvRenderer, P6: CsvRenderer, P7: CsvRenderer, P8: CsvRenderer, P9: CsvRenderer, P10: CsvRenderer, P11: CsvRenderer, P12: CsvRenderer, P13: CsvRenderer, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T)(implicit ca: CsvAttributes): CsvRenderer[T] = new BaseCsvRenderer[T] {
    val csvAttributes: CsvAttributes = ca

    /**
     * Extracts the elements of a given instance `t` of type `T` and returns them as a sequence of strings.
     * This includes rendering the 13th product element (`P13`) and combining it with a sequence of other rendered elements.
     *
     * @param t The instance of type `T` for which the elements need to be extracted and rendered.
     * @return A sequence of strings representing the rendered elements of the given instance `t`.
     */
    def elements(t: T): Strings = {
      val p13 = t.productElement(12).asInstanceOf[P13]
      val constructFirst: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T = construct(_, _, _, _, _, _, _, _, _, _, _, _, p13)
      val sequenceFirst = renderer12(constructFirst).asInstanceOf[BaseCsvRenderer[T]].elements(t)
      sequenceFirst :+ implicitly[CsvRenderer[P13]].render(p13)
    }
  }

  /**
   * Method to return a CsvRenderer[T] where T is a 13-ary Product and which is based on the given "construct" function.
   *
   * TESTME
   *
   * @param construct     a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13) => T, usually the apply method of a case class.
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
   * @tparam P13 the type of the thirteenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a CsvProduct[T].
   */
  def rendererGenerator13[P1: CsvRenderer : CsvGenerator, P2: CsvRenderer : CsvGenerator, P3: CsvRenderer : CsvGenerator, P4: CsvRenderer : CsvGenerator, P5: CsvRenderer : CsvGenerator, P6: CsvRenderer : CsvGenerator, P7: CsvRenderer : CsvGenerator, P8: CsvRenderer : CsvGenerator, P9: CsvRenderer : CsvGenerator, P10: CsvRenderer : CsvGenerator, P11: CsvRenderer : CsvGenerator, P12: CsvRenderer : CsvGenerator, P13: CsvRenderer : CsvGenerator, T <: Product : ClassTag]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T)(implicit csvAttributes: CsvAttributes): CsvProduct[T] = new ProductCsvRenderer[T] {
    def elements(t: T): Strings = renderer13(construct).asInstanceOf[BaseCsvRenderer[T]].elements(t)

    def toColumnNames(po: Option[String], no: Option[String]): String =
      new CsvGenerators {}.generator13(construct).toColumnNames(po, no)
  }

}

/**
 * The `CsvRenderers` object provides a collection of implicit `StandardCsvRenderer` implementations
 * for common data types. These renderers are used to convert data of specific types into CSV
 * (Comma-Separated Values) format according to the defined rendering logic.
 */
object CsvRenderers {
  /**
   * The `StandardCsvRenderer` is an abstract class that provides a base implementation of the
   * `CsvRenderer` trait for rendering objects of type `T` into CSV (Comma-Separated Values) format.
   *
   * This class is typically extended to create concrete renderers for specific types. By default,
   * it implements the `render` method to simply convert the input object to its string representation,
   * but this behavior may be overridden in subclasses to implement more specific serialization logic.
   *
   * @tparam T The type of object to be rendered into CSV format.
   */
  abstract class StandardCsvRenderer[T] extends CsvRenderer[T] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    /**
     * Renders an object of type `T` into its string representation.
     * Additional attributes can be specified via a map, but they are not used in the default implementation.
     *
     * @param t     the object of type `T` to be rendered.
     * @param attrs a map of attributes influencing the rendering process; not used in the default implementation.
     * @return a string representation of the object `t`.
     */
    def render(t: T, attrs: Map[String, String]): String = t.toString
  }

  /**
   * An implicit object for rendering `Boolean` values into CSV (Comma-Separated Values) format.
   *
   * `CsvRendererBoolean` extends the `StandardCsvRenderer` abstraction, which provides
   * a base implementation for converting objects into their CSV string representation.
   *
   * This implicit object handles the rendering of `Boolean` values by converting them
   * to their string representation, typically as `"true"` or `"false"`.
   *
   * Usage:
   * The `CsvRendererBoolean` can be implicitly used wherever a `StandardCsvRenderer[Boolean]`
   * is required, enabling seamless conversion of `Boolean` values to CSV-compatible strings.
   *
   * Example:
   * {{{
   * // Assuming an implicit `CsvAttributes` in scope
   * val renderer = implicitly[StandardCsvRenderer[Boolean]]
   * val csvValue = renderer.render(true, Map.empty)
   * println(csvValue) // Output: "true"
   * }}}
   */
  implicit object CsvRendererBoolean extends StandardCsvRenderer[Boolean]

  /**
   * Implicit object `CsvRendererInt` provides a concrete implementation of the `StandardCsvRenderer`
   * class for rendering `Int` values into CSV (Comma-Separated Values) format.
   *
   * This object allows the rendering of integer values into their string representation for use
   * in CSV output while adhering to the conventions defined in the `StandardCsvRenderer` abstraction.
   *
   * Being defined as implicit, it can be automatically utilized in any context where a
   * `StandardCsvRenderer[Int]` is required, enabling seamless integration into systems that
   * rely on type class-based serialization for CSV export.
   */
  implicit object CsvRendererInt extends StandardCsvRenderer[Int]

  /**
   * Implicit object `CsvRendererLong` provides a `StandardCsvRenderer` implementation specifically for the `Long` type.
   *
   * This renderer is used to convert `Long` values into their CSV (Comma-Separated Values) string representation.
   * It extends the `StandardCsvRenderer` abstract class, which defines a default rendering mechanism by
   * converting the input object to its `String` representation.
   *
   * As an implicit object, `CsvRendererLong` can be automatically applied wherever a `StandardCsvRenderer[Long]`
   * is required in the context of CSV rendering operations, such as serializing data of type `Long` into CSV format.
   *
   * Example usage:
   * {{{
   * implicit val csvAttributes: CsvAttributes = CsvAttributes() // Example of CSV configuration
   * val longValue: Long = 123456789L
   * val csvString: String = CsvRendererLong.render(longValue, Map.empty) // "123456789"
   * }}}
   *
   * Note: Additional attributes passed in the `Map` parameter to the `render` method are supported but not
   * utilized in this implementation.
   */
  implicit object CsvRendererLong extends StandardCsvRenderer[Long]

  /**
   * `CsvRendererDouble` is an implicit object that provides a mechanism to render instances
   * of type `Double` into a CSV (Comma-Separated Values) format. It extends the
   * `StandardCsvRenderer` abstract class, inheriting its generic rendering capabilities,
   * and specializes the functionality for `Double` values.
   *
   * This implicit object allows seamless and type-safe rendering of `Double`s into strings
   * formatted for CSV output by leveraging Scala's implicit resolution mechanism.
   *
   * Example usage:
   * {{{
   * import CsvRenderers._
   *
   * val value: Double = 123.456
   * val renderedValue: String = CsvRendererDouble.render(value)
   *
   * println(renderedValue) // Output: "123.456"
   * }}}
   *
   * The `CsvRendererDouble` relies on the default implementation of `render` in the
   * `StandardCsvRenderer` class, where the `Double` is converted to its string
   * representation. Custom attributes influencing the rendering may be passed via a map,
   * though they are unused in the default implementation.
   *
   * @see [[com.phasmidsoftware.tableparser.core.render.StandardCsvRenderer]]
   * @see [[com.phasmidsoftware.tableparser.core.render.Renderer]]
   */
  implicit object CsvRendererDouble extends StandardCsvRenderer[Double]

  /**
   * `CsvRendererString` is an implicit object that extends the `StandardCsvRenderer` class
   * to provide a concrete implementation of CSV rendering for `String` objects.
   *
   * This class allows a `String` to be rendered into its CSV representation. By default,
   * it inherits the behavior of the `StandardCsvRenderer`, which typically renders the input
   * by converting it to a string representation (for strings, this is simply the value itself).
   *
   * Example Usage:
   * {{{
   *   val csvString: String = CsvRendererString.render("example")
   * }}}
   */
  implicit object CsvRendererString extends StandardCsvRenderer[String]

  /**
   * Provides an implicit instance of `StandardCsvRenderer` for the type `java.net.URL`.
   *
   * This object is used to render `URL` instances into their CSV (Comma-Separated Values) string representation.
   * It extends the `StandardCsvRenderer` class, leveraging its default implementation for converting objects
   * into a string format. Specifically, for `URL` instances, the `toString` method of the URL is used as the
   * rendered string by default, unless overridden.
   *
   * The renderer is intended for scenarios where `URL` objects need to be serialized or exported in CSV format,
   * commonly for data processing or storage purposes.
   *
   * Example usage:
   * {{{
   * val url: URL = new URL("https://example.com")
   * val renderedCsv: String = implicitly[StandardCsvRenderer[URL]].render(url, Map())
   * // renderedCsv would contain "https://example.com"
   * }}}
   *
   * This implicit object allows seamless usage of `URL` type rendering in contexts where `StandardCsvRenderer`
   * is required, such as within collections or for pipeline-based CSV serialization.
   */
  implicit object CsvRendererURL extends StandardCsvRenderer[URL]
}
