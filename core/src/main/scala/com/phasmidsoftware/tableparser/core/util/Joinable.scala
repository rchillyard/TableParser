/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.util

/**
 * Type class to define behavior of a type that can be joined.
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

/**
 * Companion object for the `Joinable` type class, providing default implementations
 * of the `Joinable` behavior for specific types.
 */
object Joinable {
  /**
   * A trait extending the behavior of `Joinable` for a tuple of type `(T, Int)`.
   * It defines how to join two `(T, Int)` values, a zero value, and validity checks
   * based on the underlying `Joinable` instance for `T`.
   *
   * @tparam T the type to be joined within the `(T, Int)` tuple.
   */
  trait JoinableTInt[T] extends Joinable[(T, Int)] {

    def tj: Joinable[T]

    def join(t1: (T, Int), t2: (T, Int)): (T, Int) =
      tj.join(t1._1, t2._1) -> (if (t1._2 >= 0) t1._2 else t2._2)

    val zero: (T, Int) =
      tj.zero -> -1

    def valid(t: (T, Int)): Boolean =
      tj.valid(t._1)
  }

  /**
   * Provides an implicit implementation of the `Joinable` type class for the `String` type.
   * This enables strings to be combined using the `join` method, defines a zero value for
   * strings, and validates non-empty strings.
   */
  implicit object JoinableString extends Joinable[String] {
    /**
     * Concatenates two strings into a single string.
     *
     * @param t1 The first string to be joined.
     * @param t2 The second string to be joined.
     * @return A single string resulting from the concatenation of `t1` and `t2`.
     */
    def join(t1: String, t2: String): String =
      t1 + t2

    /**
     * Represents the zero (identity) value for the `String` type in the context of
     * the `Joinable` type class. In this case, the zero value is an empty string (`""`),
     * which acts as the neutral element in string concatenation operations.
     */
    val zero: String =
      ""

    /**
     * Validates whether the given string is non-empty.
     *
     * @param t The string to validate.
     * @return `true` if the string is non-empty, `false` otherwise.
     */
    def valid(t: String): Boolean =
      t.nonEmpty
  }

  /**
   * Provides an implicit implementation of the `Joinable` type class for sequences of strings (`Seq[String]`).
   * This enables sequences of strings to be combined, defines a zero value for sequences, and validates
   * non-empty sequences.
   */
  implicit object JoinableStrings extends Joinable[Seq[String]] {
    /**
     * Combines two sequences of strings into a single sequence by appending the elements of the second sequence
     * to the elements of the first sequence.
     *
     * @param t1 The first sequence of strings.
     * @param t2 The second sequence of strings to be appended to the first sequence.
     * @return A new sequence of strings containing all the elements of `t1`, followed by all the elements of `t2`.
     */
    def join(t1: Seq[String], t2: Seq[String]): Seq[String] =
      t1 ++ t2 // TESTME

    /**
     * Defines the zero value for sequences of strings (`Seq[String]`).
     * The zero value is represented by an empty sequence (`Nil`) and serves as the identity element
     * for the `join` operation in the `Joinable` type class, allowing seamless combination of string
     * sequences while preserving the associative and identity properties.
     */
    val zero: Seq[String] =
      Nil

    /**
     * Validates if the provided sequence of strings is non-empty.
     *
     * @param t The sequence of strings to validate.
     * @return `true` if the sequence is non-empty, otherwise `false`.
     */
    def valid(t: Seq[String]): Boolean =
      t.nonEmpty // TESTME
  }
}
