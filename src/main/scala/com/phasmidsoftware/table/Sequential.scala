package com.phasmidsoftware.table

import com.phasmidsoftware.parse.CellParser
import com.phasmidsoftware.render.CsvProduct
import com.phasmidsoftware.table.Sequence.SequenceOrdering
import scala.util.{Success, Try}

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
   * TESTME Need to test this.
   * CONSIDER it might throw compare contract exception.
   *
   * @tparam T the underlying type.
   * @return an Ordering[T] which always treats everything as the same.
   */
  def randomOrdering[T]: Ordering[T] = (x: T, y: T) => 0

  /**
   * Method to create an Ordering for type T based on an element of type P.
   *
   * @param f lens function to retrieve a P from a T.
   * @tparam T the underlying type of the elements to be ordered.
   * @tparam P the underlying type of the key element.
   * @return an Ordering[T]
   */
  def ordering[T, P: Ordering](f: T => P): Ordering[T] = (x: T, y: T) => {
    implicit val po = implicitly[Ordering[P]]
    po.compare(f(x), f(y))
  }

  /**
   * Method to create an Ordering for type T based on an optional element of type P.
   *
   * NOTE: this is more complex than it seems to require but if we allow all non-Some/Some cases to return 0,
   * we get a Contract exception.
   *
   * TODO Create a new type-class which extends Ordering but has only the zero additional method (to be used instead of Numeric).
   *
   * @param f lens function to retrieve an Option[P] from a T.
   * @tparam T the underlying type of the elements to be ordered.
   * @tparam P the underlying type of the (optional) key element.
   * @return an Ordering[T]
   */
  def optionalOrdering[T, P: Numeric](f: T => Option[P]): Ordering[T] = (x: T, y: T) => {
    implicit val po = implicitly[Numeric[P]]
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
   *
   * TESTME write this like optionOrdering.
   *
   * @param f lens function to retrieve an Option[P] from a T.
   * @tparam T the underlying type of the elements to be ordered.
   * @tparam P the underlying type of the (optional) key element.
   * @return an Ordering[T]
   */
  def tryOrdering[T, P: Ordering](f: T => Try[P]): Ordering[T] = (x: T, y: T) => {
    implicit val po = implicitly[Ordering[P]]
    (f(x), f(y)) match {
      case (Success(a), Success(b)) => po.compare(a, b)
      case _ => 0
    }
  }

}