package com.phasmidsoftware.tableparser.core.util

import com.phasmidsoftware.visitor.core.PrioQueue
import scala.annotation.tailrec

/**
 * An immutable accumulator of the n best values seen so far: the greatest (TopN.greatest) or the least (TopN.least).
 * It holds at most n values, however many it is given, so it can find, for example, the ten best rows of a stream
 * which is far too big for memory.
 *
 * It keeps a priority queue (Visitor's PrioQueue, which is a binary heap) whose head is the worst value it holds.
 * When it is full, a new value which is no better than the head is ignored; otherwise, it replaces the head.
 * So each value costs O(log n) at most.
 * Values which are equal (by the Ordering) are not kept in any particular order.
 *
 * @param n        the most values to keep.
 * @param queue    the values kept, worst first.
 * @param ordering the Ordering of the queue: a value which is lteq the queue's head is no better than it.
 * @tparam T the type of the values.
 */
final case class TopN[T] private(n: Int, private val queue: PrioQueue[T])(private val ordering: Ordering[T]):

  /**
   * Method to offer a value.
   *
   * @param t the value.
   * @return a TopN[T] which keeps t if it's one of the n best values seen so far.
   */
  infix def :+(t: T): TopN[T] =
    if n == 0 || (queue.size == n && ordering.lteq(t, queue.head)) then this
    else
      val q = queue.offer(t)
      copy(queue = if q.size > n then q.take._2 else q)(ordering)

  /**
   * Method to offer several values.
   *
   * @param ts the values.
   * @return a TopN[T] which keeps the n best of the values seen so far.
   */
  infix def ++(ts: IterableOnce[T]): TopN[T] =
    ts.iterator.foldLeft(this)(_ :+ _)

  /**
   * The number of values kept (at most n).
   */
  def size: Int = queue.size

  /**
   * The values kept, best first.
   */
  def toSeq: Seq[T] =
    // NOTE: the queue gives the worst value first, so prepending gives a List with the best first.
    @tailrec
    def inner(q: PrioQueue[T], result: List[T]): List[T] =
      if q.isEmpty then result
      else
        val (t, r) = q.take
        inner(r, t :: result)

    inner(queue, Nil)

object TopN:
  /**
   * Method to make an empty TopN which keeps the n greatest values.
   *
   * @param n        the most values to keep (not negative).
   * @param ordering implicit Ordering[T].
   * @tparam T the type of the values.
   * @return an empty TopN[T].
   */
  def greatest[T](n: Int)(using ordering: Ordering[T]): TopN[T] =
    require(n >= 0, s"TopN: n must not be negative: $n")
    TopN(n, PrioQueue.empty[T])(ordering)

  /**
   * Method to make an empty TopN which keeps the n least values.
   *
   * @param n        the most values to keep (not negative).
   * @param ordering implicit Ordering[T].
   * @tparam T the type of the values.
   * @return an empty TopN[T].
   */
  def least[T](n: Int)(using ordering: Ordering[T]): TopN[T] =
    require(n >= 0, s"TopN: n must not be negative: $n")
    TopN(n, PrioQueue.emptyMax[T])(ordering.reverse)
