package com.phasmidsoftware.table

import com.phasmidsoftware.parse.CellParser
import com.phasmidsoftware.render.CsvProduct
import com.phasmidsoftware.table.Sequence.SequenceOrdering
import scala.util.{Failure, Success, Try}

trait Sequential {
  val sequence: Sequence
}

object Sequential {
  def ordering[T <: Sequential]: Ordering[T] = (x: T, y: T) =>
    SequenceOrdering.compare(x.sequence, y.sequence)
}

class Sequence(val n: Long) extends AnyVal {
  def next: Sequence = new Sequence(n + 1)

  override def toString: String = n.toString
}

object Sequence {
  // NOTE this is unashamedly using a var.
  // CONSIDER alternative strategies to avoid use of var.
  var sequence: Sequence = Sequence(0L)

  def apply(x: Long): Sequence = new Sequence(x)

  // CONSIDER is the following actually necessary?
  implicit object SequenceOrdering extends Ordering[Sequence] {
    def compare(x: Sequence, y: Sequence): Int = implicitly[Ordering[Long]].compare(x.n, y.n)
  }

  implicit object SequenceCellParser extends CellParser[Sequence] {
    def convertString(w: String): Try[Sequence] = triedSequence

    def parse(wo: Option[String], row: Row, columns: Header): Try[Sequence] = triedSequence
  }

  implicit object SequenceCvsRenderer extends CsvProduct[Sequence] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(t: Sequence, attrs: Map[String, String]): String = s"#$t"

    def toColumnName(po: Option[String], name: String): String = ""
  }

  private def triedSequence = Try {
    sequence = sequence.next
    sequence
  }
}

object NonSequential {

  /**
   * Method to create an Ordering for type T based on an element of type P.
   *
   * @param f lens function to retrieve a P from a T.
   * @tparam T the underlying type of the elements to be ordered.
   * @tparam P the underlying type of the key element.
   * @return an Ordering[T]
   */
  def ordering[T, P: Ordering](f: T => P): Ordering[T] = (x: T, y: T) =>
    implicitly[Ordering[P]].compare(f(x), f(y))

  /**
   * Method to create an Ordering for type T based on an optional element of type P.
   *
   * NOTE: this is more complex than it seems to require but if we allow all non-Some/Some cases to return 0,
   * we get a Contract exception.
   *
   * @param f lens function to retrieve an Option[P] from a T.
   * @tparam T the underlying type of the elements to be ordered.
   * @tparam P the underlying type of the (optional) key element.
   * @return an Ordering[T]
   */
  def optionalOrdering[T, P: OrderingWithZero](f: T => Option[P]): Ordering[T] = (x: T, y: T) => {
    implicit val po = implicitly[OrderingWithZero[P]] // XXX You should ignore the request to add a type annotation here.
    (f(x), f(y)) match {
      case (Some(a), Some(b)) =>
        po.compare(a, b)
      case (Some(a), None) =>
        po.compare(a, po.zero)
      case (None, Some(b)) =>
        po.compare(po.zero, b)
      case _ =>
        0
    }
  }

  /**
   * Method to create an Ordering for type T based on a tried element of type P.
   * See comments on optionalOrdering (above).
   *
   * @param f lens function to retrieve an Try[P] from a T.
   * @tparam T the underlying type of the elements to be ordered.
   * @tparam P the underlying type of the (tried) key element.
   * @return an Ordering[T]
   */
  def tryOrdering[T, P: OrderingWithZero](f: T => Try[P]): Ordering[T] = (x: T, y: T) => {
    val po = implicitly[OrderingWithZero[P]]
    (f(x), f(y)) match {
      case (Success(a), Success(b)) =>
        po.compare(a, b)
      case (Success(a), Failure(_)) =>
        po.compare(a, po.zero)
      case (Failure(_), Success(b)) =>
        po.compare(po.zero, b)
      case _ =>
        0
    }
  }

}

trait OrderingWithZero[X] extends Ordering[X] {
  def zero: X
}

object OrderingWithZero {

  implicit object OrderingWithZeroString extends OrderingWithZero[String] {
    def zero: String = ""

    def compare(x: String, y: String): Int = x.compareTo(y)
  }

  implicit object OrderingWithZeroBoolean extends OrderingWithZero[Boolean] {
    def zero: Boolean = false

    def compare(x: Boolean, y: Boolean): Int = x.compare(y)
  }

  /**
   * Implicit method to convert an implicit Numeric[X] into an OrderingWithZero[X] for use with
   * optionOrdering and tryOrdering methods.
   *
   * @tparam X the underlying type (must have evidence of Numeric[X]).
   * @return an OrderingWithZero[X]
   */
  implicit def convert[X: Numeric]: OrderingWithZero[X] = new OrderingWithZeroFromNumeric[X] {}

  private abstract class OrderingWithZeroFromNumeric[X: Numeric] extends OrderingWithZero[X] {
    def zero: X = implicitly[Numeric[X]].zero

    def compare(x: X, y: X): Int = implicitly[Numeric[X]].compare(x, y)
  }
}