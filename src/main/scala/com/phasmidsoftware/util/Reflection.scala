/*
 * This module derives from Spray.
 */

package com.phasmidsoftware.util

import java.lang.reflect.Modifier

import scala.reflect.ClassTag
import scala.util.control.NonFatal

object Reflection {

  /**
    * This method is borrowed directly from Spray ProductFormats.
    *
    * NOTE: Read this if you are getting exceptions thrown by this method (be aware that sys.error throws an exception).
    * You MUST be careful when defining case classes that represent input types or output types.
    * DO NOT use val or lazy val instead of def for additional instance methods.
    * That is because they mess up the matching of the fields.
    *
    * @param classTag rhw class tag.
    * @return an Array of String.
    */
  def extractFieldNames(classTag: ClassTag[_]): Array[String] = {
    val clazz = classTag.runtimeClass
    try {
      // NOTE: copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
      // ... that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
      val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
        _.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)
      val fields = clazz.getDeclaredFields.filterNot { f =>
        import Modifier._
        (f.getModifiers & (TRANSIENT | STATIC | 0x1000 /* SYNTHETIC*/)) > 0
      }
      if (copyDefaultMethods.length != fields.length)
        sys.error("Case class " + clazz.getName + " declares additional fields: Did you use val instead of def for a method in this case class?")
      if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
        sys.error("Cannot determine field order of case class " + clazz.getName + ": Did you use val instead of def for a method in this case class?")
      fields.map(f => f.getName)
    } catch {
      case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
        "for '" + clazz.getName + "' (Did you use val instead of def for a method in this case class?), please use the 'cellParser'N signature with explicit field name specification", ex)
    }
  }

}
