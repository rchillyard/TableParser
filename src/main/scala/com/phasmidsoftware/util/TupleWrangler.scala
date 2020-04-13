package com.phasmidsoftware.util

/**
  * A collection of methods to manipulate tuples.
  */
object TupleWrangler {

  /**
    * Method to unpack a nested tuple of form ((A, B), C) into a non-nested tuple: (A, B, C).
    * This is particularly useful if zipping for example xs zip ys zip zs.
    *
    * @param t a nested 2-tuple of form ((A, B), C).
    * @tparam A the underlying type of the first element.
    * @tparam B the underlying type of the first element.
    * @tparam C the underlying type of the first element.
    * @return a 3-tuple of form (A, B, C).
    */
  def inline[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, T](f: (P0, P1) => T): P0 => T = p0 => p0.asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, T](f: (P0, P1, P2) => T): (P0, P1) => T = (p0, p1) => (p0, p1).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, T](f: (P0, P1, P2, P3) => T): (P0, P1, P2) => T = (p0, p1, p2) => (p0, p1, p2).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam P4 the 4th type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, T](f: (P0, P1, P2, P3, P4) => T): (P0, P1, P2, P3) => T = (p0, p1, p2, p3) => (p0, p1, p2, p3).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam P4 the 4th type referenced by f.
    * @tparam P5 the 5th type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, T](f: (P0, P1, P2, P3, P4, P5) => T): (P0, P1, P2, P3, P4) => T = (p0, p1, p2, p3, p4) => (p0, p1, p2, p3, p4).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam P4 the 4th type referenced by f.
    * @tparam P5 the 5th type referenced by f.
    * @tparam P6 the 6th type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, P6, T](f: (P0, P1, P2, P3, P4, P5, P6) => T): (P0, P1, P2, P3, P4, P5) => T = (p0, p1, p2, p3, p4, p5) => (p0, p1, p2, p3, p4, p5).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam P4 the 4th type referenced by f.
    * @tparam P5 the 5th type referenced by f.
    * @tparam P6 the 6th type referenced by f.
    * @tparam P7 the 7th type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, P6, P7, T](f: (P0, P1, P2, P3, P4, P5, P6, P7) => T): (P0, P1, P2, P3, P4, P5, P6) => T = (p0, p1, p2, p3, p4, p5, p6) => (p0, p1, p2, p3, p4, p5, p6).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam P4 the 4th type referenced by f.
    * @tparam P5 the 5th type referenced by f.
    * @tparam P6 the 6th type referenced by f.
    * @tparam P7 the 7th type referenced by f.
    * @tparam P8 the 8th type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, P6, P7, P8, T](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8) => T): (P0, P1, P2, P3, P4, P5, P6, P7) => T = (p0, p1, p2, p3, p4, p5, p6, p7) => (p0, p1, p2, p3, p4, p5, p6, p7).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0 the 0th type referenced by f.
    * @tparam P1 the 1st type referenced by f.
    * @tparam P2 the 2nd type referenced by f.
    * @tparam P3 the 3rd type referenced by f.
    * @tparam P4 the 4th type referenced by f.
    * @tparam P5 the 5th type referenced by f.
    * @tparam P6 the 6th type referenced by f.
    * @tparam P7 the 7th type referenced by f.
    * @tparam P8 the 8th type referenced by f.
    * @tparam P9 the 9th type referenced by f.
    * @tparam T  the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, T](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): (P0, P1, P2, P3, P4, P5, P6, P7, P8) => T = (p0, p1, p2, p3, p4, p5, p6, p7, p8) => (p0, p1, p2, p3, p4, p5, p6, p7, p8).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0  the 0th type referenced by f.
    * @tparam P1  the 1st type referenced by f.
    * @tparam P2  the 2nd type referenced by f.
    * @tparam P3  the 3rd type referenced by f.
    * @tparam P4  the 4th type referenced by f.
    * @tparam P5  the 5th type referenced by f.
    * @tparam P6  the 6th type referenced by f.
    * @tparam P7  the 7th type referenced by f.
    * @tparam P8  the 8th type referenced by f.
    * @tparam P9  the 9th type referenced by f.
    * @tparam P10 the 10th type referenced by f.
    * @tparam T   the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, T](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9) => T = (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) => (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9).asInstanceOf[T]

  /**
    * Recast function f into a function with one fewer parameters.
    * This method should not be used if the function f is ever to be invoked directly.
    *
    * @param f the function to be recast.
    * @tparam P0  the 0th type referenced by f.
    * @tparam P1  the 1st type referenced by f.
    * @tparam P2  the 2nd type referenced by f.
    * @tparam P3  the 3rd type referenced by f.
    * @tparam P4  the 4th type referenced by f.
    * @tparam P5  the 5th type referenced by f.
    * @tparam P6  the 6th type referenced by f.
    * @tparam P7  the 7th type referenced by f.
    * @tparam P8  the 8th type referenced by f.
    * @tparam P9  the 9th type referenced by f.
    * @tparam P10 the 10th type referenced by f.
    * @tparam P11 the 11th type referenced by f.
    * @tparam T   the target type for both f and the result.
    * @return a new function which has one fewer parameters.
    */
  def strip[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, T](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T = (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) => (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10).asInstanceOf[T]

}
