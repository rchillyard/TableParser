package com.phasmidsoftware.table

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParIterable
import scala.reflect.ClassTag

/**
 * Class to represent the rows of a Table.
 * NOTE: do not expect an iterator on Content to be in any particular order.
 *
 * At present, the rows are implemented as a ParIterable.
 * However, we might later change the internal representation, thus xs is private.
 *
 * CONSIDER making the private val parameter an Either of ParIterable[Row] or Iterable[Row].
 * That's to say lazy/parallelized vs. eager.
 * Take care, however, as both extend GenIterable[Row].
 *
 * @param xs a ParIterable[Row].
 * @tparam Row the underlying Row type.
 */
case class Content[+Row](private val xs: ParIterable[Row]) {

  def size: Int = xs.size

  def knownSize: Int = xs.knownSize

  def toSeq: Seq[Row] = xs.to(Seq)

  def toIndexedSeq: IndexedSeq[Row] = xs.to(IndexedSeq)

  def toArray[B >: Row : ClassTag]: Array[B] = xs.toArray

  def iterator: Iterator[Row] = xs.toIterator

  def foreach(f: Row => Unit): Unit = xs foreach f

  def filter(p: Row => Boolean): Content[Row] = Content(xs filter p)

  def filterNot(p: Row => Boolean): Content[Row] = Content(xs filterNot p)

  def map[B](f: Row => B): Content[B] = Content(xs map f)

  /**
   * This is not, strictly speaking, the correct definition of flatMap for allowing Content to be a monad.
   *
   * @param f a function of type Row=>ParIterable[B]
   * @tparam B the underlying type of the result.
   * @return a Content[B].
   */
  def flatMap[B](f: Row => ParIterable[B]): Content[B] = Content(xs flatMap f)

  /**
   * Transform (flatMap) this Table[Row] into a Table[S].
   *
   * @param f a function which transforms a Row into an IterableOnce[S].
   * @tparam S the type of the rows of the result.
   * @return a Table[S] which is made up of a concatenation of the results of invoking f on each row this
   */
  def mapOptional[S](f: Row => Option[S]): Content[S] =
    Content(for (q <- xs.map(f); r <- q) yield r)

  def foldLeft[B](z: B)(op: (B, Row) => B): B = xs.foldLeft(z)(op)

  /**
   * Method to concatenate two Contents.
   * CONSIDER is this a source of inefficiency?
   *
   * @param other the other Content.
   * @tparam B the underlying type of the other Content and the result. Must be a super-type of Row.
   * @return Content[B].
   */
  def ++[B >: Row](other: Content[B]): Content[B] = Content(xs ++ other.xs)

  def drop(n: Int): Content[Row] = Content(xs drop n)

  def dropWhile(p: Row => Boolean): Content[Row] = Content(xs dropWhile p)

  def take(n: Int): Content[Row] = Content(xs take n)

  def takeWhile(p: Row => Boolean): Content[Row] = Content(xs takeWhile p)

  def slice(from: Int, until: Int): Content[Row] = Content(xs.slice(from, until))

  /**
   * This should be used only by unit tests and not be code.
   *
   * @return the first element of xs.
   */
  def head: Row = xs.head

  /**
   * Method to transform this Content[Row] into a sorted Content[S] where S is a super-class of Row and for which there is
   * evidence of Ordering[S].
   *
   * @tparam S the underlying type of the resulting Table (a super-type of Row and for which there is evidence of Ordering[S]).
   * @return a Content[S].
   */
  def sorted[S >: Row : Ordering]: Content[S] = Content(toIndexedSeq.map(_.asInstanceOf[S]).sorted)

  /**
   * Method to transform this Content[Row] into a sorted Seq[S] where S is a super-class of Row and for which there is
   * evidence of Ordering[S].
   *
   * @tparam S the underlying type of the resulting Table (a super-type of Row and for which there is evidence of Ordering[S]).
   * @return a Seq[S].
   */
  def ordered[S >: Row : Ordering]: Seq[S] = toSeq.map(_.asInstanceOf[S]).sorted

}

object Content {
  def apply[T](xs: Iterable[T]): Content[T] = Content(xs.par)
}
