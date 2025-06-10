package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvGenerators}
import scala.util.{Failure, Success, Try}

/**
 * This class is used for the situation where a column in a table actually contains a set of
 * attributes, typically separated by "," and possibly bracketed by "{}".
 * CONSIDER allowing "|" as a separator (as described previously in the documentation here).
 *
 * @param xs the attribute values.
 */
case class AttributeSet(xs: StringList)

/**
 * The `AttributeSet` object provides utility methods for constructing and parsing instances
 * of the `AttributeSet` class. This is primarily used for handling a column in a table that
 * represents a set of attributes, typically formatted as a comma-separated list, and
 * potentially enclosed in brackets or other separators.
 */
object AttributeSet extends CellParsers with CsvGenerators {

  /**
   * This method is required to be a String=>AttributeSet and is only invoked inside Try.
   * It invokes parse to get its result.
   *
   * NOTE: essentially, we are doing a get, and trying to make it explicit so that Codacy doesn't get upset ;)
   *
   * @param w the String to be converted to an AttributeSet.
   * @return an AttributeSet.
   */
  def apply(w: String): AttributeSet = parse(w) match {
    case Success(a) =>
      a
    case Failure(x) =>
      throw x
  }

  /**
   * Method to parse a String as an AttributeSet.
   *
   * @param w the String to be parsed as an AttributeSet.
   * @return a Try[AttributeSet]
   */
  def parse(w: String): Try[AttributeSet] =
    Parseable.split(w).map(apply)

  val none: AttributeSet = AttributeSet(Nil)

  implicit val parser: CellParser[AttributeSet] = cellParser(AttributeSet.apply: String => AttributeSet)
  val fAttributeSet: StringList => AttributeSet = AttributeSet.apply
  implicit val generatorAttributeSet: CsvGenerator[AttributeSet] = generator1(fAttributeSet)
}

