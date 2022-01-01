/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

/**
 * Type class to define behavior of a type which can be joined.
 *
 * @tparam T the joinable type.
 */
trait Joinable[T] {
  /**
   * Method to join t1 to t2.
   *
   * @param t1 the first T value.
   * @param t2 the second T value.
   * @return the joined T value.
   */
  def join(t1: T, t2: T): T

  /**
   * The zero value for T.
   */
  val zero: T

  /**
   * Method to determine if t is valid.
   *
   * @param t a T value.
   * @return true if t is valid.
   */
  def valid(t: T): Boolean
}

object Joinable {
  implicit object JoinableString extends Joinable[String] {
    def join(t1: String, t2: String): String = t1 + t2

    val zero: String = ""

    def valid(t: String): Boolean = t.nonEmpty
  }

  implicit object JoinableStrings extends Joinable[Seq[String]] {
    def join(t1: Seq[String], t2: Seq[String]): Seq[String] = t1 ++ t2

    val zero: Seq[String] = Nil

    def valid(t: Seq[String]): Boolean = t.nonEmpty
  }
}
