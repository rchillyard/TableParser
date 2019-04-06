/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import scala.reflect.ClassTag

object Reflection {

	/**
		* This method is borrowed directly from Spray JsonReader
		*
		* @param classTag rhw class tag.
		* @return an Array of String.
		*/
	def extractFieldNames(classTag: ClassTag[_]): Array[String] = {
		import java.lang.reflect.Modifier

		import scala.util.control.NonFatal

		val clazz = classTag.runtimeClass
		try {
			// copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
			// that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
			val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
				_.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)
			val fields = clazz.getDeclaredFields.filterNot { f =>
				import Modifier._
				(f.getModifiers & (TRANSIENT | STATIC | 0x1000 /* SYNTHETIC*/)) > 0
			}
			if (copyDefaultMethods.length != fields.length)
				sys.error("Case class " + clazz.getName + " declares additional fields")
			if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
				sys.error("Cannot determine field order of case class " + clazz.getName)
			fields.map(f => f.getName)
		} catch {
			case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
				"for '" + clazz.getName + "', please use the 'jsonFormat' overload with explicit field name specification", ex)
		}
	}

}
