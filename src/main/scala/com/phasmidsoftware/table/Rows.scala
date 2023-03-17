package com.phasmidsoftware.table

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParIterable
import scala.reflect.ClassTag

case class Rows[+Row](xs: ParIterable[Row]) {

  def size: Int = xs.size

  def knownSize: Int = xs.knownSize

  def toSeq: Seq[Row] = xs.to(List)

  def toIndexedSeq: IndexedSeq[Row] = xs.to(IndexedSeq)

  def toArray[B >: Row : ClassTag]: Array[B] = xs.toArray

  def iterator: Iterator[Row] = xs.toIterator

  def foreach(f: Row => Unit): Unit = xs foreach f

  def filter(p: Row => Boolean): Rows[Row] = Rows(xs filter p)

  def filterNot(p: Row => Boolean): Rows[Row] = Rows(xs filterNot p)

  def map[B](f: Row => B): Rows[B] = Rows(xs map f)

  /**
   * This is not, strictly speaking, the correct definition of flatMap for allowing Rows to be a monad.
   *
   * @param f a function of type Row=>ParIterable[B]
   * @tparam B the underlying type of the result.
   * @return a Rows[B].
   */
  def flatMap[B](f: Row => ParIterable[B]): Rows[B] = Rows(xs flatMap f)

  def foldLeft[B](z: B)(op: (B, Row) => B): B = xs.foldLeft(z)(op)

  def ++[B >: Row](other: Rows[B]): Rows[B] = Rows(xs ++ other.xs)

  def drop(n: Int): Rows[Row] = Rows(xs drop n)

  def dropWhile(p: Row => Boolean): Rows[Row] = Rows(xs dropWhile p)

  def take(n: Int): Rows[Row] = Rows(xs take n)

  def takeWhile(p: Row => Boolean): Rows[Row] = Rows(xs takeWhile p)

  def slice(from: Int, until: Int): Rows[Row] = Rows(xs.slice(from, until))

  /**
   * This should be used only by unit tests and not be code.
   *
   * @return the first element of xs.
   */
  def head: Row = xs.head
}

object Rows {
  def apply[T](xs: Iterable[T]): Rows[T] = Rows(xs.par)
}
