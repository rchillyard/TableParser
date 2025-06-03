package com.phasmidsoftware.tableparser.core.table

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
 * @param xs a ParIterable[Row].
 * @tparam Row the underlying Row type.
 */
case class Content[+Row](private val xs: ParIterable[Row]) {

  /**
   * Method to get the number of elements in the underlying collection `xs`.
   *
   * @return the size of the collection as an integer.
   */
  def size: Int =
    xs.size

  /**
   * Returns the known size of the collection underlying this `Content` instance.
   * If the size is known, the method will return the exact size. Otherwise, it will return -1.
   *
   * @return the known size of the collection, or -1 if the size cannot be determined.
   */
  def knownSize: Int =
    xs.knownSize

  /**
   * Converts the underlying collection of rows to a sequence.
   *
   * @return a sequence (Seq[Row]) containing the rows of the content.
   */
  def toSeq: Seq[Row] =
    xs.to(Seq)

  /**
   * Converts the content of `xs` into an `IndexedSeq[Row]`.
   *
   * @return an IndexedSeq containing all elements of `xs`.
   */
  def toIndexedSeq: IndexedSeq[Row] =
    xs.to(IndexedSeq)

  /**
   * Converts the underlying collection of elements to an array.
   *
   * @tparam B the type of elements in the resulting array. It must be a supertype of `Row` and have an implicit `ClassTag`.
   * @return an array containing all elements of the collection.
   */
  def toArray[B >: Row : ClassTag]: Array[B] =
    xs.toArray

  /**
   * Provides an iterator to traverse the rows in this content.
   *
   * @return an `Iterator` over the `Row` elements of this content in their current order.
   */
  def iterator: Iterator[Row] =
    xs.toIterator

  /**
   * Applies the given function `f` to each `Row` element in the collection.
   *
   * This method iterates over all elements in the underlying sequence `xs`
   * and applies the specified function `f` to each of them.
   *
   * @param f a function of type `Row => Unit` that is applied to each row in the collection.
   *          The function is invoked for its side effects on each element.
   * @return `Unit` as this method is intended for side-effectful operations.
   */
  def foreach(f: Row => Unit): Unit =
    xs foreach f

  /**
   * Filters the rows in this content based on the given predicate.
   *
   * @param p a predicate function that determines whether a given Row should be included in the resulting Content.
   *          The function should return true for rows that should be included and false otherwise.
   * @return a new Content[Row] containing only the rows from this content that satisfy the given predicate.
   */
  def filter(p: Row => Boolean): Content[Row] =
    Content(xs filter p)

  /**
   * Filters the rows of this `Content[Row]` by removing those that satisfy the given predicate function.
   *
   * @param p the predicate function that determines whether a `Row` should be excluded.
   *          Rows for which this function evaluates to true will be excluded from the resulting `Content`.
   * @return a new `Content[Row]` containing only the rows that do not satisfy the given predicate function.
   */
  def filterNot(p: Row => Boolean): Content[Row] =
    Content(xs filterNot p)

  /**
   * Transforms each element in the current `Content` collection using the given function `f`.
   *
   * @param f A function that takes an element of type `Row` and produces a new element of type `B`.
   * @return A new `Content` instance containing the transformed elements of type `B`.
   */
  def map[B](f: Row => B): Content[B] =
    Content(xs map f)

  /**
   * This is not, strictly speaking, the correct definition of flatMap for allowing Content to be a monad.
   *
   * @param f a function of type Row=>ParIterable[B]
   * @tparam B the underlying type of the result.
   * @return a Content[B].
   */
  def flatMap[B](f: Row => ParIterable[B]): Content[B] =
    Content(xs flatMap f)

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
   * Applies a binary operator to a start value and all elements of the underlying
   * collection `xs`, going left to right, and returns a reduced value.
   *
   * This method performs a fold operation, where the initial value is provided by `z`
   * and the binary operator `op` is applied successively to combine elements of the
   * collection with the accumulated result.
   *
   * @param z  The initial value of the fold operation.
   * @param op A binary operator function that takes the current accumulated result
   *           and the next element of the collection, returning a new accumulated result.
   *           The function has the type `(B, Row) => B`, where `B` is the type of the
   *           accumulated result, and `Row` represents elements of the collection.
   * @return The result of applying the fold operation over all elements of the collection.
   *         The result type is `B`.
   */
  def foldLeft[B](z: B)(op: (B, Row) => B): B =
    xs.foldLeft(z)(op)

  /**
   * Method to concatenate two Contents.
   * CONSIDER perhaps we should use the combiner functionality more directly?
   * (It's what the ++ method uses).
   *
   * @param other the other Content.
   * @tparam B the underlying type of the other Content and the result. Must be a super-type of Row.
   * @return Content[B].
   */
  def ++[B >: Row](other: Content[B]): Content[B] =
    Content(xs ++ other.xs)

  /**
   * Drops the first `n` elements from the underlying collection `xs` and returns a new `Content` instance
   * containing the remaining elements.
   *
   * The method does not modify the original collection but instead creates a new `Content` instance
   * with the result of the `drop` operation applied to `xs`.
   *
   * @param n the number of elements to drop from the start of the collection.
   *          If `n` is greater than the size of the collection, an empty `Content` is returned.
   * @return a new `Content[Row]` containing the elements of the collection after dropping the first `n` elements.
   */
  def drop(n: Int): Content[Row] =
    Content(xs drop n)

  /**
   * Drops elements from this `Content[Row]` collection as long as the given predicate `p`
   * evaluates to true, and returns the remaining elements wrapped in a new `Content`.
   *
   * The elements are checked in their order, and once the predicate returns false for the
   * first time, no further elements are dropped, and the remaining elements are included
   * in the resulting `Content`.
   *
   * @param p a predicate function of type `Row => Boolean`. This function determines whether
   *          an element should be dropped. Elements for which the predicate returns true
   *          are removed from the start of the collection.
   * @return a new `Content[Row]` containing the elements of this `Content` after all
   *         leading elements satisfying the predicate `p` have been removed.
   */
  def dropWhile(p: Row => Boolean): Content[Row] =
    Content(xs dropWhile p)

  /**
   * Returns a new `Content[Row]` containing the first `n` elements from the current `Content[Row]`.
   *
   * If `n` is greater than the size of the underlying collection, all elements from the collection
   * will be included in the returned `Content[Row]`. If `n` is less than or equal to zero, the result
   * will be an empty `Content[Row]`.
   *
   * @param n the number of elements to take from the beginning of the underlying collection.
   *          Must be a non-negative integer.
   * @return a new `Content[Row]` containing up to the first `n` elements from the current content.
   */
  def take(n: Int): Content[Row] =
    Content(xs take n)

  /**
   * Selects elements from the beginning of the underlying collection of rows, as long as they satisfy the specified predicate.
   * The operation stops as soon as a row is encountered that does not meet the condition.
   *
   * @param p a predicate function of type `Row => Boolean` that determines whether a row should be included.
   *          The function should return `true` to select the row and `false` to stop further selection.
   * @return a new `Content[Row]` containing the rows from the start of the collection that satisfy the given predicate.
   */
  def takeWhile(p: Row => Boolean): Content[Row] =
    Content(xs takeWhile p)

  /**
   * Produces a slice of the content by extracting elements within the specified range.
   *
   * This method creates a new `Content[Row]` that contains elements from index `from` (inclusive)
   * to index `until` (exclusive) of the underlying collection `xs`. If the range specified by
   * `from` and `until` exceeds the bounds of the underlying collection, the slicing will
   * silently handle it and only include the valid indices.
   *
   * @param from  the starting index (inclusive) of the slice.
   * @param until the ending index (exclusive) of the slice.
   * @return a new `Content[Row]` containing the elements in the specified range.
   */
  def slice(from: Int, until: Int): Content[Row] =
    Content(xs.slice(from, until))

  /**
   * This should be used only by unit tests and not be code.
   *
   * @return the first element of xs.
   */
  def head: Row =
    xs.head

  /**
   * Method to transform this Content[Row] into a sorted Content[S] where S is a super-class of Row and for which there is
   * evidence of Ordering[S].
   *
   * @tparam S the underlying type of the resulting Table (a super-type of Row and for which there is evidence of Ordering[S]).
   * @return a Content[S].
   */
  def sorted[S >: Row : Ordering]: Content[S] =
    Content(toIndexedSeq.map(_.asInstanceOf[S]).sorted)
}

/**
 * The `Content` object provides a factory method for creating instances of the `Content` class.
 * This utility is designed to facilitate the creation of `Content` objects from standard iterable collections
 * by converting them into parallel collections for potential parallel processing.
 */
object Content {
  /**
   * Creates an instance of the `Content` class by converting the given iterable collection
   * into a parallel collection.
   *
   * @param xs the iterable collection to be converted into a parallel collection
   * @return an instance of `Content` containing the parallelized collection
   */
  def apply[T](xs: Iterable[T]): Content[T] = Content(xs.par)
}
