/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import java.io.File
import java.net.URL
import org.joda.time.LocalDate
import scala.annotation.implicitNotFound
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

/**
 * Type class which describes a type which can be parsed from a String.
 *
 * @tparam T the resulting type.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of Parseable[${T}]. This is unusual when your application types are all case classes. Most of the standard types are supported in the Parseable companion object. Take a look and define something similar that works for your type, or consider redefining your type as a case class.")
trait Parseable[T] {

  /**
   * Parse a String as a Try[T].
   *
   * @param s the String to be parsed.
   * @return the corresponding value of type T, wrapped in Try.
   */
  def parse(s: String, optModifier: Option[String] = None): Try[T]
}

/**
 * The `Parseable` object provides a set of predefined `Parseable` type class instances that
 * enable parsing of `String` representations into instances of various data types. These
 * parsers operate based on the `Parseable[T]` trait, which defines a `parse` method that
 * returns a `Try[T]`.
 *
 * This framework is designed to handle common data parsing scenarios with support for
 * additional parsing behaviors through optional modifiers.
 *
 * The parsers handle parsing errors gracefully, returning a `Failure` instance with an
 * appropriate exception message when parsing fails.
 */
object Parseable {

  /**
   * Parser of String.
   * The exception is useful for ensuring a None in the case of an optional String.
   */
  trait ParseableString extends Parseable[String] {
    /**
     * Parses the given input string and applies an optional modifier.
     * If the input string is empty, it returns a failure with a `BlankException`.
     * Otherwise, it returns the input string wrapped in a success.
     *
     * @param s           the input string to be parsed. Should not be empty.
     * @param optModifier an optional modifier that could be applied during the parse operation.
     *                    This parameter is not utilized in the current implementation.
     * @return a `Success` containing the input string if it's non-empty, or a `Failure` containing a `BlankException` if the string is empty.
     */
    def parse(s: String, optModifier: Option[String]): Try[String] =
      if (s.isEmpty) Failure(BlankException()) else Success(s)
  }

  implicit object ParseableString extends ParseableString

  /**
   * Parser of Boolean.
   */
  trait ParseableBoolean extends Parseable[Boolean] {
    /**
     * Parses the given string `s` into a `Boolean`, optionally applying `optModifier`.
     * The method attempts to convert the string to a `Boolean` using `parseAndRecover`,
     * which provides recovery logic for invalid inputs by wrapping them in appropriate exceptions.
     *
     * @param s           the input string to be parsed into a `Boolean`.
     * @param optModifier an optional modifier that can affect the parsing process (not utilized in the current implementation).
     * @return a `Try` containing `Boolean` on success, or a `Failure` with an appropriate exception on error.
     */
    def parse(s: String, optModifier: Option[String]): Try[Boolean] =
      parseAndRecover(s)(lift(_.toBoolean))(w => s"ParseableBoolean: cannot interpret '$w' as a Boolean")
  }

  implicit object ParseableBoolean extends ParseableBoolean

  /**
   * Parser of Byte.
   */
  trait ParseableByte extends Parseable[Byte] {
    /**
     * Parses the provided string input (`s`) into a `Byte` value safely, with optional modifications based on `optModifier`.
     * In case of failure, it recovers with an error message indicating the inability to parse the value.
     *
     * @param s           the string input to be parsed into a `Byte`.
     * @param optModifier an optional string modifier that can influence the parsing (currently unused in this implementation).
     * @return a `Try[Byte]` which contains the successfully parsed `Byte` value, or a `Failure` with relevant error information if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[Byte] =
      parseAndRecover(s)(lift(_.toByte))(w => s"ParseableByte: cannot interpret '$w' as a Byte")
  }

  implicit object ParseableByte extends ParseableByte

  /**
   * Parser of Char.
   */
  trait ParseableChar extends Parseable[Char] {
    /**
     * Parses the given string `s` to extract the first character, optionally modified by `optModifier`.
     * If parsing fails, a descriptive error message is returned in the failure.
     *
     * @param s           the input string to be parsed. It must not be empty if the parsing is to succeed.
     * @param optModifier an optional modifier string that may influence the parsing. Currently unused.
     * @return a `Try[Char]` that contains a `Char` if parsing is successful, or a failure describing the error.
     */
    def parse(s: String, optModifier: Option[String]): Try[Char] =
      parseAndRecover(s)(lift(_.head))(w => s"ParseableChar: cannot interpret '$w' as a Char")
  }

  implicit object ParseableChar extends ParseableChar

  /**
   * Parser of Short.
   */
  trait ParseableShort extends Parseable[Short] {
    /**
     * Parses the given string into a `Short`, applying optional modifications if provided.
     * If the string cannot be parsed, a descriptive error message will be returned encapsulated in a `Failure`.
     *
     * @param s           the input string to parse.
     * @param optModifier an optional modifier string that can influence parsing behavior (not utilized explicitly in this implementation).
     * @return a `Try[Short]` which is a `Success` containing the parsed `Short` if successful, or a `Failure` containing an error if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[Short] =
      parseAndRecover(s)(lift(_.toShort))(w => s"ParseableShort: cannot interpret '$w' as a Short")
  }

  implicit object ParseableShort extends ParseableShort

  /**
   * Parser of Int.
   */
  trait ParseableInt extends Parseable[Int] {
    /**
     * Parses a given string into an integer while optionally considering a modifier.
     * CONSIDER supporting different radix
     *
     * This method attempts to interpret the input string `s` using the specified transformation logic.
     * If the input cannot be parsed into an integer, it falls back to a recovery mechanism that
     * produces an appropriate error message.
     *
     * @param s           the string input to parse as an integer.
     * @param optModifier an optional modifier that could influence parsing behavior or interpretation, though it is currently unused.
     * @return a `Try` of `Int`, which will be a `Success` wrapping the integer if parsing is successful,
     *         or a `Failure` if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[Int] =
      parseAndRecover(s)(lift(_.toInt))(w => s"ParseableInt: cannot interpret '$w' as an Int")
  }

  implicit object ParseableInt extends ParseableInt

  /**
   * Parser of Long.
   */
  trait ParseableLong extends Parseable[Long] {
    /**
     * Parses a given string to a `Long`, optionally applying a modifier, and returns the result as a `Try`.
     * CONSIDER supporting different radix
     *
     * @param s           the input string to be parsed.
     * @param optModifier an optional string modifier that can influence the parsing process.
     * @return a `Try[Long]` containing the parsed value if successful, or a failure with an appropriate exception if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[Long] =
      parseAndRecover(s)(lift(_.toLong))(w => s"ParseableLong: cannot interpret '$w' as a Long")
  }

  implicit object ParseableLong extends ParseableLong

  /**
   * Parser of BigInt.
   */
  trait ParseableBigInt extends Parseable[BigInt] {
    /**
     * Parses the input string into a BigInt using the specified radix (base), determined by the optional modifier.
     * If the optional modifier is provided, it is converted to an integer and used as the radix. If not provided, a default radix of 10 is used.
     *
     * @param s           the input string to be parsed into a BigInt.
     * @param optModifier an optional string specifying the radix (base) to use for parsing. If not provided, the default radix is 10.
     * @return a `Try[BigInt]` representing the result of parsing. The `Try` will contain a `BigInt` if parsing succeeds,
     *         or a `Failure` with an appropriate exception if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[BigInt] = {
      val radix = optModifier map (_.toInt) getOrElse 10
      val f: String => BigInt = BigInt(_, radix)
      parseAndRecover(s)(lift(f))(w => s"ParseableBigInt: cannot interpret '$w' in radix $radix as a BigInt")
    }
  }

  implicit object ParseableBigInt extends ParseableBigInt

  /**
   * Parser of BigDecimal.
   */
  trait ParseableBigDecimal extends Parseable[BigDecimal] {
    /**
     * Parses a string into a `BigDecimal` with optional modification logic. If parsing fails, it attempts to recover
     * and provides a meaningful error message, wrapped in a `Failure`.
     *
     * @param s           the string to parse.
     * @param optModifier an optional modifier that may influence the parsing logic.
     * @return a `Try` containing the parsed `BigDecimal` if successful, or a `Failure` with an error message wrapped in an exception.
     */
    def parse(s: String, optModifier: Option[String]): Try[BigDecimal] =
      parseAndRecover(s)(lift(BigDecimal.apply))(w => s"ParseableBigDecimal: cannot interpret '$w' as a BigDecimal")
  }

  implicit object ParseableBigDecimal extends ParseableBigDecimal

  /**
   * Parser of Double.
   */
  trait ParseableDouble extends Parseable[Double] {
    /**
     * Parses the given string into a `Double`, optionally using a modifier to adjust parsing behavior.
     * If the parsing fails due to an invalid `Double` format or the string being empty, a failure with an appropriate exception is returned.
     *
     * @param s           the input string to be parsed.
     * @param optModifier an optional string modifier that might affect the parsing logic (though it is unused in this implementation).
     * @return a `Try[Double]` representing the successful parse result or a failure if parsing is invalid.
     */
    def parse(s: String, optModifier: Option[String]): Try[Double] =
      parseAndRecover(s)(lift(_.toDouble))(w => s"ParseableDouble: cannot interpret '$w' as a Double")
  }

  implicit object ParseableDouble extends ParseableDouble

  /**
   * Parser of Float.
   */
  trait ParseableFloat extends Parseable[Float] {
    /**
     * Parses a string into a Float, with optional modification, and handles potential parsing errors gracefully.
     *
     * @param s           the string to be parsed, expected to represent a valid Float value.
     * @param optModifier an optional modifier that might influence the parsing process (currently unused in this method).
     * @return a `Try[Float]` containing the parsed Float if successful, or a Failure containing the relevant exception if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[Float] =
      parseAndRecover(s)(lift(_.toFloat))(w => s"ParseableFloat: cannot interpret '$w' as a Float")
  }

  implicit object ParseableFloat extends ParseableFloat

  /**
   * Parser of LocalDate.
   */
  trait ParseableLocalDate extends Parseable[LocalDate] {
    /**
     * Parses a given string into a `LocalDate` object.
     * Attempts to parse the input string using the `LocalDate.parse` method, with optional modifications
     * provided by the `optModifier`. If parsing fails, it recovers with a failure including an appropriate error message.
     *
     * @param s           the input string to be parsed into a `LocalDate`.
     * @param optModifier an optional modifier that could alter the parsing logic (not utilized in this implementation).
     * @return a `Try[LocalDate]` that will contain a `LocalDate` if parsing succeeds, or a `Failure` if it fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[LocalDate] =
      parseAndRecover(s)(lift(LocalDate.parse))(w => s"ParseableLocalDate: cannot interpret '$w' as a LocalDate")
  }

  implicit object ParseableLocalDate extends ParseableLocalDate

  /**
   * Parser of URL.
   */
  trait ParseableURL extends Parseable[URL] {
    /**
     * Parses a given string into a `URL` object, with an optional modifier for customization.
     *
     * @param s           the input string to be parsed; it should represent a valid URL.
     * @param optModifier an optional string modifier that can be used to customize the parsing behavior.
     * @return a `Try[URL]` which contains the successfully parsed `URL` in case of success,
     *         or a failure wrapped in `Try` if the input string cannot be interpreted as a valid URL,
     *         with an appropriate error message.
     */
    def parse(s: String, optModifier: Option[String]): Try[URL] =
      parseAndRecover(s)(lift(new URL(_)))(w => s"ParseableURL: cannot interpret '$w' as an URL")
  }

  implicit object ParseableURL extends ParseableURL

  /**
   * Parser of File.
   */
  trait ParseableFile extends Parseable[File] {
    /**
     * Parses a given string into a `File` object while optionally considering a modifier.
     *
     * The method attempts to create a `File` from the provided string `s`.
     * If parsing fails due to an invalid input, a failure is returned with an appropriate error message.
     *
     * @param s           the input string to be parsed into a `File`.
     * @param optModifier an optional string modifier which may affect the parsing logic
     *                    (the modifier is currently unused in the implementation but reserved for potential future use).
     * @return a `Try[File]` which will contain the parsed `File` if successful,
     *         or a `Failure` with an error message if parsing fails.
     */
    def parse(s: String, optModifier: Option[String]): Try[File] =
      parseAndRecover(s)(lift(new File(_)))(w => s"ParseableFile: cannot interpret '$w' as a File")
  }

  implicit object ParseableFile extends ParseableFile

  /**
   * Parser of StringList.
   * This trait splits strings of the form {x,y,z}, regardless of the format specified by the RowConfig object.
   */
  trait ParseableStringList extends Parseable[StringList] {
    /**
     * Parses the given string into a `StringList`, optionally applying a modifier.
     *
     * @param s           the input string to parse.
     * @param optModifier an optional string modifier to influence the parsing logic.
     * @return a `Try[StringList]` which is successful if parsing succeeds, otherwise a failure if the string cannot
     *         be interpreted as a `StringList`.
     */
    def parse(s: String, optModifier: Option[String]): Try[StringList] =
      parseAndRecover(s)(split)(w => s"ParseableStringList: cannot interpret '$w' as a StringList")
  }

  implicit object ParseableStringList extends ParseableStringList

  /**
   * Method to split a String into a StringList, parser.list.
   *
   * @param w the String to parse.
   * @return a Try[StringList].
   */
  def split(w: String): Try[StringList] =
    ListParser.parseAll(ListParser.list, w) match {
      case ListParser.Success(ws: StringList, _) =>
        Success(ws)
      case ListParser.Failure(msg, _) =>
        Failure(ParseLogicException(s"cannot split string '$w': $msg"))
      case ListParser.Error(msg, _) =>
        Failure(ParseLogicException(s"cannot split string '$w': $msg"))
      case _ =>
        Failure(ParseLogicException(s"cannot split string '$w'"))
  }

  private def lift[T](f: String => T): String => Try[T] = w => Try(f(w))

  private def parseAndRecover[T](w: String)(f: String => Try[T])(msg: String => String): Try[T] =
    f(w).recoverWith {
      case x: IllegalArgumentException =>
        Failure(if (w.nonEmpty) InvalidParseException(msg(w), x) else BlankException(x))
//      case x: NumberFormatException =>
//        Failure(if (w.nonEmpty) InvalidParseException(msg(w), x) else BlankException(x))
    }
}

/**
 * Abstract class to parse optional scala values.
 *
 * @tparam T the resulting type for which there must be evidence of a Parseable[T].
 */
abstract class ParseableOption[T: Parseable] extends Parseable[Option[T]] {
  /**
   * Parses a given string into an optional value of type T, handling blank input as None.
   *
   * This method utilizes an implicit `Parseable[T]` to parse the input string. If the string
   * is deemed blank (causing a `BlankException`), the result will be a `Success(None)`.
   * Otherwise, it will attempt to parse the string into a value of type T and wrap the result
   * in an `Option`.
   *
   * @param s           the input string to be parsed.
   * @param optModifier an optional modifier that may influence parsing behavior.
   * @return a `Try` containing an optional value of type T if parsing succeeds,
   *         or an error if parsing fails for other exceptions.
   */
  def parse(s: String, optModifier: Option[String]): Try[Option[T]] =
    implicitly[Parseable[T]].parse(s, optModifier).map(Option(_)).recoverWith {
    case _: BlankException => Success(None)
  }
}

/**
 * The `ParseableOption` object provides implicit instances of `ParseableOption`
 * for various types, enabling the parsing of optional Scala values.
 *
 * `ParseableOption` works in conjunction with the abstract class of the same name,
 * allowing strings to be parsed into optional values of specified types while handling
 * blanks as `None`. This is achieved through the underlying `Parseable` evidence available
 * for the corresponding type.
 *
 * The implicit objects defined here provide type-specific implementations
 * for common data types like `String`, `Int`, `Boolean`, `LocalDate`, and more.
 *
 * These implicit objects are particularly useful in scenarios involving generic parsing
 * where the target type is optional, and blanks should not raise errors but return `None`.
 *
 * Example:
 * {{{
 * import com.phasmidsoftware.tableparser.core.parse._
 * import ParseableOption._
 *
 * val maybeInt: Try[Option[Int]] = implicitly[Parseable[Option[Int]]].parse("42", None)
 * val maybeNone: Try[Option[Int]] = implicitly[Parseable[Option[Int]]].parse("", None)
 * }}}
 *
 * The implicit instances provided:
 * - `ParseableOptionString`: Parses optional `String`
 * - `ParseableOptionBoolean`: Parses optional `Boolean`
 * - `ParseableOptionByte`: Parses optional `Byte`
 * - `ParseableOptionShort`: Parses optional `Short`
 * - `ParseableOptionInt`: Parses optional `Int`
 * - `ParseableOptionFloat`: Parses optional `Float`
 * - `ParseableOptionDouble`: Parses optional `Double`
 * - `ParseableOptionLong`: Parses optional `Long`
 * - `ParseableOptionBigInt`: Parses optional `BigInt`
 * - `ParseableOptionBigDecimal`: Parses optional `BigDecimal`
 * - `ParseableOptionLocalDate`: Parses optional `LocalDate`
 * - `ParseableOptionURL`: Parses optional `java.net.URL`
 * - `ParseableOptionFile`: Parses optional `java.io.File`
 */
object ParseableOption {

  implicit object ParseableOptionString extends ParseableOption[String]

  implicit object ParseableOptionBoolean extends ParseableOption[Boolean]

  implicit object ParseableOptionByte extends ParseableOption[Byte]

  implicit object ParseableOptionShort extends ParseableOption[Short]

  implicit object ParseableOptionInt extends ParseableOption[Int]

  implicit object ParseableOptionFloat extends ParseableOption[Float]

  implicit object ParseableOptionDouble extends ParseableOption[Double]

  implicit object ParseableOptionLong extends ParseableOption[Long]

  implicit object ParseableOptionBigInt extends ParseableOption[BigInt]

  implicit object ParseableOptionBigDecimal extends ParseableOption[BigDecimal]

  implicit object ParseableOptionLocalDate extends ParseableOption[LocalDate]

  implicit object ParseableOptionURL extends ParseableOption[URL]

  implicit object ParseableOptionFile extends ParseableOption[File]

}

/**
 * This object is a parser of lists.
 * A list is considered to be enclosed by {} and separated by commas.
 */
object ListParser extends JavaTokenParsers {

  /**
   * A lazy parser for `StringList` which represents a list of strings.
   *
   * This parser supports two main formats:
   * 1. A list enclosed by curly braces `{}` containing comma-separated strings.
   * For example: `"{a, b, c}"` would be parsed as `List("a", "b", "c")`.
   * 2. A single string (singleton element) without the list enclosure.
   * For example: `"element"` would be parsed as `List("element")`.
   *
   * This parser combines the following parsers:
   * - `strings`: Parses a list of comma-separated strings within `{}`.
   * - `singleton`: Parses a single standalone string.
   *
   * The combination is achieved using the alternation operator (`|`).
   */
  lazy val list: Parser[StringList] =
    "{" ~> strings <~ "}" | singleton

  /**
   * A parser for a list of strings separated by commas.
   *
   * This parser matches sequences of strings that do not include a comma or a closing curly brace `}`.
   * Each string is separated by a comma `,`. The parser outputs the parsed values as a `StringList`
   * (a type alias for `List[String]`).
   *
   * Example:
   * - Input: `"a,b,c"`
   * - Output: `List("a", "b", "c")`
   *
   * - Input: `"}a,b,c}"`
   * - Not valid because of mismatched braces, handled elsewhere.
   *
   * Used in: Parsing lists enclosed by `{}` as part of the `list` parser in ListParser.
   */
  lazy val strings: Parser[StringList] =
    repsep("""[^,}]+""".r, ",")

  /**
   * A lazy-initialized parser for a singleton value in the form of a `StringList`.
   *
   * The singleton is defined as matching a word-like string using the regex `\w*`,
   * which represents zero or more word characters (letters, digits, or underscores).
   * When a match is found, it is converted into a single-element `StringList`,
   * which is synonymous with `List[String]`, containing only the matched word.
   *
   * This parser serves as a fallback or alternative option for parsing a minimal
   * list of one word in contexts where broader parsing rules might not apply.
   *
   * Example:
   * - Input: `"example"`
   * - Output: `List("example")`
   */
  private lazy val singleton: Parser[StringList] =
    """\w*""".r ^^ { (w: String) => List(w) }
}

/**
 * Exception class representing an error encountered during parsing logic.
 *
 * This exception is used to indicate situations where parsing fails due to some issue
 * in the business or logical rules during the process of parsing data.
 *
 * @param msg A descriptive message providing information about the error. This message
 *            is used to help diagnose the cause of the parsing failure.
 * @param e   The underlying cause of the exception, if any. Defaults to `null` if
 *            no specific cause is provided.
 */
case class ParseLogicException(msg: String, e: Throwable = null) extends Exception(msg, e)

/**
 * Abstract class `ParseableException` is an extension of the standard Scala `Exception` class
 * and provides a base structure for defining custom exceptions that can carry a parse error message
 * and an optional cause (a `Throwable`).
 *
 * This class can be used as a foundation for defining exceptions when parsing operations fail
 * or encounter errors, allowing informative and structured error reporting.
 *
 * @param msg the error message detailing the cause or nature of the parsing issue.
 * @param e   an optional throwable that caused this exception, defaulting to `null` if not provided.
 */
abstract class ParseableException(msg: String, e: Throwable = null) extends Exception(msg, e)

/**
 * A case class representing an exception that occurs when a parsing error is encountered
 * and the parsing operation fails to produce a valid result.
 *
 * This exception is a specialized form of `ParseableException`.
 *
 * @param msg A string message providing details about the parsing error.
 * @param e   An optional `Throwable` cause for the exception (default is `null`).
 * @constructor Creates an instance of `InvalidParseException`.
 */
case class InvalidParseException(msg: String, e: Throwable = null) extends ParseableException(msg, e)

/**
 * A case class representing a specific exception type `BlankException` used when a blank value or an error associated with
 * a blank value is encountered during the parsing process. It extends `ParseableException` as its base type.
 *
 * @param e an optional `Throwable` cause associated with the exception. Defaults to `null` if not provided.
 * @constructor Creates a new `BlankException` by delegating to the base `ParseableException` and providing
 *              the message identifier "blank" along with the optional cause `e`.
 * @example
 * {{{
 * // Throwing a BlankException with a cause
 * throw BlankException(new IllegalArgumentException("Blank value encountered"))
 *
 * // Throwing a BlankException without a cause
 * throw BlankException()
 * }}}
 */
case class BlankException(e: Throwable = null) extends ParseableException("blank", e)
