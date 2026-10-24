/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.write.Node
import java.time.LocalDate
import scala.annotation.implicitNotFound
import com.phasmidsoftware.tableparser.core.util.Derivation
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

/**
 * Definition of trait Renderer for the purpose of serializing objects of type T as an object of type O.
 * This trait may be used as a type class for either T or O (or both).
 *
 * NOTE: this trait has no direct relationship with TableRenderable.
 *
 * @tparam T the (contravariant) type of object to be rendered.
 * @tparam O the type of the serialization result.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of Renderer[${T},${O}].")
trait Renderer[-T, O] {

  /**
   * Render an instance of T as an O, qualifying the rendering with no attributes.
   *
   * @param t a T to be rendered.
   * @return an instance of type O.
   */
  def render(t: T): O = render(t, Map())

  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t     the input parameter, i.e. the T object to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: T, attrs: Map[String, String]): O
}

/**
 * Definition of type class HierarchicalRenderer for the purpose of serializing objects of type T.
 * Since, in general, T will be a case class which may include parameters which are case classes,
 * we render to a hierarchical type: Node.
 *
 * @tparam T the type of object to be rendered.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of HierarchicalRenderer[${T}].")
trait HierarchicalRenderer[T] extends Renderer[T, Node] {

  /**
   * Defines the default style for type T.
   */
  val style: String

  /**
   * Defines the base attribute set for type T.
   */
  val baseAttrs: Map[String, String] = Map()

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node

  /**
   * Method to render content as a String.
   * This method is invoked only when T is not a Product, sequence or Option.
   * Normally, the default method is what is required, but it might be necessary to override
   * in some situations.
   * This method does not apply to style or attribute values.
   *
   * @param t the content value.
   * @return a String corresponding to t.
   */
  def asString(t: T): String = t.toString
}

/**
 * CONSIDER having style defined as an Option[String]
 *
 * @tparam T the type of object to be rendered.
 */
trait UntaggedHierarchicalRenderer[T] extends HierarchicalRenderer[T] {
  val style: String = ""

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)

}

/**
 * Abstract class TaggedHierarchicalRenderer.
 *
 * @param style     the style (a String)
 * @param baseAttrs baseAttrs (a Map[String, String]
 * @tparam T the type of object to be rendered.
 */
abstract class TaggedHierarchicalRenderer[T](val style: String, override val baseAttrs: Map[String, String] = Map()) extends HierarchicalRenderer[T] {

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)
}

/**
 * Abstract class ProductHierarchicalRenderer
 *
 * @param style     the style (a String)
 * @param baseAttrs baseAttrs (a Map[String, String]
 * @tparam T the type of object to be rendered.
 */
abstract class ProductHierarchicalRenderer[T](val style: String, override val baseAttrs: Map[String, String] = Map()) extends HierarchicalRenderer[T] {
  def render(t: T, attrs: Map[String, String]): Node = Node(style, attrs, nodes(t))

  protected def nodes(t: T): Seq[Node]
}

/**
 * The HierarchicalRenderer of a case class T, as derived by HierarchicalRenderer.derived.
 *
 * @param style          the style of the node for a T.
 * @param attrs          the base attributes of the node for a T.
 * @param fieldNames     the names of the fields of T, in declaration order.
 * @param fieldRenderers the HierarchicalRenderers of the fields of T, in the same order.
 * @tparam T the type of the case class.
 */
class ProductFieldsHierarchicalRenderer[T](style: String, attrs: Map[String, String], fieldNames: List[String], fieldRenderers: List[HierarchicalRenderer[?]]) extends ProductHierarchicalRenderer[T](style, attrs) {
  // NOTE the casts: t is a case class (it has a Mirror.ProductOf), and each renderer is the one for its field.
  protected def nodes(t: T): Seq[Node] =
    (t.asInstanceOf[Product].productIterator zip fieldNames zip fieldRenderers map { case ((x, name), r) => r.asInstanceOf[HierarchicalRenderer[Any]].render(x, Map("name" -> name)) }).toSeq
}

/**
 * Object `HierarchicalRenderer` provides a collection of implicit instances
 * and type definitions for the `UntaggedHierarchicalRenderer` trait, allowing
 * for hierarchical serialization of various primitive and common types such as
 * `String`, `Boolean`, `Int`, `Long`, `BigInt`, `Double`, and `LocalDate`.
 *
 * These implicit objects define how to render instances of their respective
 * types into a hierarchical structure (`Node`) by extending the generic
 * `UntaggedHierarchicalRenderer` which implements the `HierarchicalRenderer`
 * interface.
 */
object HierarchicalRenderer {

  /**
   * Method to derive a HierarchicalRenderer for a case class T, as a node with the given style whose children
   * are its fields, each rendered by its own HierarchicalRenderer, with a "name" attribute giving the field's name.
   * This replaces the renderer1 ... renderer13 methods of HierarchicalRenderers in 1.x.
   *
   * NOTE: because it takes a style, this is used as `given HierarchicalRenderer[T] = HierarchicalRenderer.derived[T]("style")`,
   * not with `derives`. Each field's type must have a HierarchicalRenderer of its own.
   *
   * NOTE: for a case class defined inside a method, see the NOTE on CellParser.derived.
   *
   * @param style the style of the node for a T.
   * @param attrs the base attributes of the node for a T (defaults to none).
   * @param m     the Mirror of T.
   * @tparam T a case class.
   * @return a HierarchicalRenderer[T].
   */
  inline def derived[T](style: String, attrs: Map[String, String] = Map())(using m: Mirror.ProductOf[T]): HierarchicalRenderer[T] =
    new ProductFieldsHierarchicalRenderer[T](style, attrs, Derivation.fieldNames[m.MirroredElemLabels], fieldRenderers[m.MirroredElemTypes])

  private inline def fieldRenderers[Ts <: Tuple]: List[HierarchicalRenderer[?]] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[HierarchicalRenderer[t]] :: fieldRenderers[ts]
  }

  /**
   * A type class that provides functionality to render hierarchical data as strings.
   *
   * The `StringHierarchicalRenderer` trait extends the `UntaggedHierarchicalRenderer` specifically
   * for rendering data where the rendered output is of type `String`. This trait is the
   * foundation for constructing string-based hierarchical renderings within a rendering framework.
   *
   * It inherits the capabilities from `UntaggedHierarchicalRenderer[String]`, where:
   * - It allows objects of type `T` to be systematically converted into a string format
   * with optional attributes that define characteristics of rendering.
   * - It contains a default `style` attribute that can be used when rendering (defaults to an empty string).
   *
   * By using `StringHierarchicalRenderer`, developers can define ways to represent data objects hierarchically
   * in string form, while benefiting from shared behavior in the `UntaggedHierarchicalRenderer` parent trait.
   *
   * @see UntaggedHierarchicalRenderer
   * @see HierarchicalRenderer
   */
  trait StringHierarchicalRenderer extends UntaggedHierarchicalRenderer[String]

  /**
   * Implicit object for rendering hierarchical structures as strings.
   *
   * The `StringHierarchicalRenderer` provides an implicit instance of the `StringHierarchicalRenderer` type class,
   * allowing hierarchical data structures to be rendered into a string format. This is useful for cases where
   * hierarchical data needs to be displayed, logged, or processed in textual form.
   *
   * A `StringHierarchicalRenderer` is a subtype of `UntaggedHierarchicalRenderer[String]`, enabling the rendering
   * of hierarchy data without requiring specific type tags.
   *
   * Usage:
   * By importing this implicit object into scope, you can provide a default mechanism for string-based hierarchical
   * rendering for types supporting the `StringHierarchicalRenderer` trait.
   */
  given StringHierarchicalRenderer: StringHierarchicalRenderer with {}

  /**
   * Trait `BooleanHierarchicalRenderer` extends the `UntaggedHierarchicalRenderer` for handling Boolean types.
   *
   * This trait provides rendering capabilities for Boolean values, which involve rendering them
   * into a structured format while potentially applying additional attributes or styles.
   * It serves as a type-specific implementation for rendering Boolean values in a hierarchical model.
   *
   * The style associated with the rendering can be customized, and attributes can be passed to the `render` method
   * for finer control over the rendered output.
   *
   * ==Type Parameters==
   * None, as this is specifically defined for the `Boolean` type.
   *
   * ==Inheritance==
   * This trait inherits from `UntaggedHierarchicalRenderer`, leveraging the base rendering functionality provided
   * by that trait while specializing it for Boolean types.
   *
   * ==Usage==
   * This trait is typically used where hierarchical rendering of Boolean values is required, such as in
   * contexts involving nested or tree-like data structures.
   *
   * An implicit instance of this trait is available, enabling seamless use in generic code that requires
   * an implicit renderer for type Boolean.
   *
   * @see UntaggedHierarchicalRenderer
   * @see HierarchicalRenderer
   */
  trait BooleanHierarchicalRenderer extends UntaggedHierarchicalRenderer[Boolean]

  /**
   * An implicit object for rendering Boolean values in a hierarchical structure.
   *
   * `BooleanHierarchicalRenderer` is an instance of the `BooleanHierarchicalRenderer` trait,
   * which provides support for rendering Boolean values by adhering to the `UntaggedHierarchicalRenderer`
   * contract. It allows for consistent and structured formatting of Boolean data to fit into
   * a hierarchical rendering context.
   *
   * This implicit instance can be automatically utilized wherever a `BooleanHierarchicalRenderer` is
   * required, making it easier to work with Boolean rendering across hierarchical structures
   * without the need for explicitly defining renderers.
   */
  given BooleanHierarchicalRenderer: BooleanHierarchicalRenderer with {}

  /**
   * A trait for rendering instances of type `Int` in a hierarchical structure, extending
   * the `UntaggedHierarchicalRenderer` functionality tailored for the `Int` type.
   *
   * This trait provides a framework for rendering integers with an associated set of attributes,
   * potentially styled through the `style` property inherited from its parent trait.
   *
   * The `IntHierarchicalRenderer` can be utilized as a part of a broader rendering pipeline where
   * structured (hierarchical) representation of data is required. This trait acts as the specific
   * renderer for `Int` values, leveraging mechanisms defined in its supertype.
   *
   * @see [[UntaggedHierarchicalRenderer]] for further details around the rendering process
   *      and expected behavior of methods like `render`.
   */
  trait IntHierarchicalRenderer extends UntaggedHierarchicalRenderer[Int]

  /**
   * Implicit object `IntHierarchicalRenderer` provides a default implementation of the `IntHierarchicalRenderer` trait,
   * which extends the `UntaggedHierarchicalRenderer` type class for the `Int` type, enabling hierarchical rendering operations
   * for `Int` values. This object acts as a companion and offers concrete behavior for rendering integers hierarchically.
   *
   * The purpose of this renderer is to support type-safe and composable rendering of `Int` values in contexts where hierarchical
   * data needs to be processed. The implementation is expected to define how integers are transformed or rendered in a hierarchical structure.
   *
   * Usage:
   * By being an implicit object, `IntHierarchicalRenderer` is automatically available wherever an `IntHierarchicalRenderer` instance
   * is required implicitly. Users can  this behavior by creating their own instances if a customized implementation is needed.
   *
   * Example:
   * {{{
   *   // Assuming a hierarchical rendering context
   *   val result = someHierarchicalRenderingMethod(42) // Uses IntHierarchicalRenderer implicitly
   * }}}
   */
  given IntHierarchicalRenderer: IntHierarchicalRenderer with {}

  /**
   * Trait `LongHierarchicalRenderer` extends `UntaggedHierarchicalRenderer` specifically for rendering
   * hierarchical structures of type `Long`. It provides a framework to transform instances of `Long`
   * into a node-based hierarchical representation, leveraging the untagged style rendering provided
   * by the parent trait.
   *
   * This trait does not define any additional members or behavior itself but inherits from
   * `UntaggedHierarchicalRenderer`, using `Long` as the predefined type parameter.
   *
   * @see UntaggedHierarchicalRenderer
   */
  trait LongHierarchicalRenderer extends UntaggedHierarchicalRenderer[Long]

  /**
   * Implicit object for rendering `Long` values in a hierarchical manner.
   *
   * This object provides an implicit instance of `LongHierarchicalRenderer` trait
   * to render `Long` type values in a structured, hierarchical format.
   * The specific rendering behavior is defined within the `LongHierarchicalRenderer` trait.
   *
   * Usage:
   * Implicitly used in contexts where a `Long` type value needs to be serialized
   * or represented hierarchically.
   */
  given LongHierarchicalRenderer: LongHierarchicalRenderer with {}

  /**
   * A type class responsible for rendering instances of `BigInt` in a hierarchical structure
   * without additional tagging or specific rendering formats.
   *
   * This trait extends the functionality provided by `UntaggedHierarchicalRenderer`,
   * specifically for the type `BigInt`. It inherits the capability of rendering hierarchical
   * structures while allowing attributes for customization.
   *
   * Since it extends `UntaggedHierarchicalRenderer`, it provides a `render` method to
   * convert a `BigInt` instance into a `Node` representation while factoring in additional
   * attributes and style information predefined in `UntaggedHierarchicalRenderer`.
   *
   * Example usage:
   * {{{
   * implicit object BigIntHierarchicalRenderer extends BigIntHierarchicalRenderer
   *
   * val bigIntInstance: BigInt = BigInt(12345)
   * val renderedNode: Node = BigIntHierarchicalRenderer.render(bigIntInstance, Map("attr1" -> "value1"))
   *
   * println(renderedNode) // Node(style, Some("12345"), Map("attr1" -> "value1"))
   * }}}
   *
   * @see UntaggedHierarchicalRenderer for more details regarding the rendering framework.
   */
  trait BigIntHierarchicalRenderer extends UntaggedHierarchicalRenderer[BigInt]

  /**
   * Implicit object for rendering `BigInt` instances hierarchically.
   *
   * This object extends the `BigIntHierarchicalRenderer` trait, which itself
   * is a specialized implementation of the `UntaggedHierarchicalRenderer` for `BigInt`.
   * It serves as an implicit instance to provide hierarchical rendering functionality
   * for the `BigInt` type.
   *
   * The hierarchical rendering defined by this object facilitates representing
   * `BigInt` values in structured or nested formats, which may be used for
   * serialization, visualization, or other purposes in applications requiring
   * hierarchical representations.
   *
   * Usage Example:
   * {{{
   * import com.example.renderers.BigIntHierarchicalRenderer
   *
   * val bigIntValue: BigInt = BigInt("12345678901234567890")
   * val renderedOutput = someRenderingMethod(bigIntValue) // Uses BigIntHierarchicalRenderer implicitly
   * println(renderedOutput)
   * }}}
   */// TESTME
  given BigIntHierarchicalRenderer: BigIntHierarchicalRenderer with {}

  /**
   * Trait `DoubleHierarchicalRenderer` extends the `UntaggedHierarchicalRenderer` to provide rendering
   * capabilities specifically for objects of type `Double` in a hierarchical structure.
   *
   * This trait is used to define how instances of `Double` can be represented in a hierarchical format,
   * inheriting the basic behavior and methods from the `UntaggedHierarchicalRenderer` type class.
   *
   * Key Features:
   * - Automatic support for rendering `Double` objects in a hierarchical layout.
   * - Includes rendering functionality inherited from `UntaggedHierarchicalRenderer`, such as the `render` method.
   * - Provides a default styling structure through the `style` property inherited from the parent trait.
   *
   * This trait simplifies the customization and usage of hierarchical renderers specific to the `Double` type
   * and can be utilized as part of a rendering pipeline where structured data representations are needed.
   *
   * @see [[UntaggedHierarchicalRenderer]] for more details on the base functionality.
   */
  trait DoubleHierarchicalRenderer extends UntaggedHierarchicalRenderer[Double]

  /**
   * Implicit object providing a default implementation of `DoubleHierarchicalRenderer`.
   *
   * The `DoubleHierarchicalRenderer` is an implicit, singleton object that serves as a type class
   * instance for rendering `Double` values in a hierarchical structure. It extends the `DoubleHierarchicalRenderer` trait,
   * which itself extends `UntaggedHierarchicalRenderer[Double]`. This ensures that the object integrates into systems
   * utilizing the `HierarchicalRenderer` type class framework.
   *
   * This implicit object allows seamless rendering of `Double` values when a hierarchical rendering
   * type class is required in the context of `HierarchicalRenderer`.
   */
  given DoubleHierarchicalRenderer: DoubleHierarchicalRenderer with {}

  /**
   * Trait `LocalDateHierarchicalRenderer` is a specialized implementation of the `UntaggedHierarchicalRenderer`
   * for rendering instances of `LocalDate` in a hierarchical structured format. It provides a mechanism
   * to render `LocalDate` objects as nodes with additional attributes.
   *
   * This trait inherits the rendering capabilities from `UntaggedHierarchicalRenderer[LocalDate]`,
   * which is designed for rendering objects of a specific type while supporting custom attributes and style.
   *
   * Example usage:
   * {{{
   *   implicit object MyLocalDateRenderer extends LocalDateHierarchicalRenderer
   *   val renderedNode: Node = MyLocalDateRenderer.render(LocalDate.now, Map("key" -> "value"))
   * }}}
   *
   * @see [[UntaggedHierarchicalRenderer]] for the base trait and its methods.
   */
  trait LocalDateHierarchicalRenderer extends UntaggedHierarchicalRenderer[LocalDate]

  /**
   * Implicit object `LocalDateHierarchicalRenderer` for rendering `LocalDate` in a hierarchical structure.
   *
   * This object extends the `LocalDateHierarchicalRenderer` trait, which specializes `UntaggedHierarchicalRenderer`
   * for rendering instances of `LocalDate`. It provides type class support for hierarchical rendering
   * of `LocalDate` objects.
   *
   * The rendering mechanism provided by this implicit object ensures that `LocalDate` instances are
   * converted into a hierarchical representation that aligns with the expected structure defined by its
   * associated renderer type class.
   *
   * This implicit is typically used for cases where `LocalDate` representation as part of a larger
   * heterogeneous data structure needs to be rendered in a textual or hierarchical format that aligns
   * with `UntaggedHierarchicalRenderer` semantics.
   *
   * Usage:
   * Simply ensure `LocalDateHierarchicalRenderer` is in scope when rendering `LocalDate` within a hierarchical
   * context, and the appropriate rendering implementation will be picked up implicitly.
   *
   * Example:
   * {{{
   * import LocalDateHierarchicalRenderer._
   *
   * val localDate: LocalDate = LocalDate.of(2023, 10, 15)
   * val rendered = summon[LocalDateHierarchicalRenderer].render(localDate, Map.empty)
   * println(rendered) // Outputs the hierarchical representation of the LocalDate
   * }}}
   */// TESTME
  given LocalDateHierarchicalRenderer: LocalDateHierarchicalRenderer with {}

//  implicit object RowStringRenderer extends Renderer[Row,String] {
//    /**
//     * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
//     *
//     * @param t     the input parameter, i.e. the T object to render.
//     * @param attrs a map of attributes for this value of O.
//     * @return an instance of type O.
//     */
//    def render(t: Row, attrs: Map[String, String]): String = t.toString()  // TESTME
//  }
}
