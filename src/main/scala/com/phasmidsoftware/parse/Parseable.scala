package com.phasmidsoftware.parse

import java.io.File
import java.net.URL

import org.joda.time.LocalDate

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Type class which describes a type which can be parsed from a String.
  *
  * @tparam T the resulting type.
  */
trait Parseable[T] {

  def parse(s: String): T

  def tryParse(s: String): Try[T] = Try(parse(s))
}

object Parseable {

  trait ParseableBoolean extends Parseable[Boolean] {
    // CONSIDER doing something about this because an illegal value throws IllegalArgumentException (not very helpful)
    // FIXME we have values that are not true/false that we have to recognize
    override def parse(s: String): Boolean = try s.toBoolean catch {
      case _: IllegalArgumentException => throw ParserException(s"ParseableBoolean: cannot interpret '$s' as a Boolean")
    }
  }

  implicit object ParseableBoolean extends ParseableBoolean

  trait ParseableInt extends Parseable[Int] {
    override def parse(s: String): Int = try s.toInt catch {
      case _: IllegalArgumentException => throw ParserException(s"ParseableInt: cannot interpret '$s' as an Int")
    }
  }

  implicit object ParseableInt extends ParseableInt

  trait ParseableLong extends Parseable[Long] {
    override def parse(s: String): Long = s.toLong
  }

  implicit object ParseableLong extends ParseableLong

  trait ParseableDouble extends Parseable[Double] {
    override def parse(s: String): Double = s.toDouble
  }

  implicit object ParseableDouble extends ParseableDouble

  trait ParseableLocalDate extends Parseable[LocalDate] {
    override def parse(s: String): LocalDate = LocalDate.parse(s)
  }

  implicit object ParseableLocalDate extends ParseableLocalDate

  trait ParseableURL extends Parseable[URL] {
    override def parse(s: String): URL = new URL(s)
  }

  implicit object ParseableURL extends ParseableURL

  trait ParseableFile extends Parseable[File] {
    override def parse(s: String): File = new File(s)
  }

  implicit object ParseableFile extends ParseableFile

  trait ParseableStringList$ extends Parseable[List[String]] {
    override def parse(s: String): List[String] = split(s)
  }

  implicit object ParseableStringList$ extends ParseableStringList$

  private val parser = new ListParser()

  private def split(w: String): List[String] = {
    parser.parseAll(parser.list, w) match {
      case parser.Success(ws: List[String], _) => ws
      case parser.Failure(msg, _) => throw ParserException(s"cannot split string '$w': $msg")
      case parser.Error(msg, _) => throw ParserException(s"cannot split string '$w': $msg")
      case _ => throw ParserException(s"cannot split string '$w'")
    }
  }
}

class ListParser() extends JavaTokenParsers {

  def list: Parser[List[String]] = "{" ~> repsep("""\w+""", ",") <~ "}"
}
