package com.phasmidsoftware.table

import com.phasmidsoftware.table.Content.noOrdering
import com.phasmidsoftware.util.FP
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParIterable
import scala.util.Random

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
 * See [[https://docs.scala-lang.org/overviews/parallel-collections/overview.html]] for more information on parallel collections.
 * However, we can note a few things here:
 * <ol>
 * <li>parallel collections remain ordered unless transformed with "bulk" operations such as map, filter;</li>
 * <li>seq is always an efficient method on parallel collections;</li>
 * <li>for now, imposition of an explicit ordering is done via sorted or ordered methods.</li>
 * </ol>
 *
 * @param xs a ParIterable[Row].
 * @tparam Row the underlying Row type.
 */
case class Content[+Row](private val xs: ParIterable[Row]) extends IterableOnce[Row] {

  def size: Int = xs.size

  def toSeq: Seq[Row] = xs.to(List)

  def toIndexedSeq: IndexedSeq[Row] = xs.toIndexedSeq

  def iterator: Iterator[Row] = xs.iterator

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
   * Method to sample from this Content by a deterministic method (every nth row is chosen).
   * NOTE: this is not random.
   *
   * @param n the number of rows from which we select the first.
   * @return a new Content[Row] with approximately size/n elements.
   */
  def step(n: Int): Content[Row] = Content(xs.seq.grouped(n).map(ys => ys.head).toSeq)

  /**
   * Method to randomly sample from this Content.
   *
   * @param n      the odds against choosing any particular element.
   * @param random an (implicit) Random number generator.
   * @return a new Content[Row] with approximately size/n elements.
   */
  def sample(n: Int)(implicit random: Random): Content[Row] = filter(FP.sampler(n))

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
   * NOTE that if the specified ordering is noOrdering, then no ordering takes place.
   *
   * @tparam S the underlying type of the resulting Table (a super-type of Row and for which there is evidence of Ordering[S]).
   * @return a Content[S].
   */
  def sorted[S >: Row : Ordering]: Content[S] =
    if (implicitly[Ordering[S]] != noOrdering)
      Content(xs.to(IndexedSeq).map(_.asInstanceOf[S]).sorted)
    else
      this

  /**
   * Method to transform this Content[Row] into a sorted Seq[S] where S is a super-class of Row and for which there is
   * evidence of Ordering[S].
   *
   * @tparam S the underlying type of the resulting Table (a super-type of Row and for which there is evidence of Ordering[S]).
   * @return a Seq[S].
   */
  def ordered[S >: Row : Ordering]: Seq[S] =
    if (implicitly[Ordering[S]] != noOrdering)
      xs.to(Seq).map(_.asInstanceOf[S]).sorted
    else
      xs.to(Seq)
}

object Content {
  def apply[T](xs: Iterable[T]): Content[T] = Content(xs.par)

  /**
   * Ordering such that all elements appear equal.
   * Ideally, this should take linear time for any adaptive sorting method such as Timsort, insertion sort, etc.
   * However, within the context of Content, we don't invoke this ordering at all if it is referenced.
   *
   * @tparam T the underlying type.
   * @return an Ordering[T] which always treats everything as the same.
   */
  def noOrdering[T]: Ordering[T] =
    (_: T, _: T) => 0
}
