package com.phasmidsoftware.parse

import scala.util.{Failure, Success, Try}

/**
 * This class is used for the situation where a column in a table actually contains a set of
 * attributes, typically separated by "," and possibly bracketed by "{}".
 * CONSIDER allowing "|" as a separator (as described previously in the documentation here).
 *
 * @param xs the attribute values.
 */
case class AttributeSet(xs: StringList)

object AttributeSet {

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
    case Success(a) => a
    case Failure(x) => throw x
  }

  /**
   * Method to parse a String as an AttributeSet.
   *
   * @param w the String to be parsed as an AttributeSet.
   * @return a Try[AttributeSet]
   */
  def parse(w: String): Try[AttributeSet] = Parseable.split(w).map(apply)
}

