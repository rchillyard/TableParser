/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render.tag

import scala.language.implicitConversions

/**
 * Case class defining an attribute.
 *
 * @param key   the attribute's key.
 * @param value the attribute's value.
 */
case class Attribute(key: String, value: String) {
  override def toString: String = s"""$key="$value""""
}

/**
 * Trait Tag to model a tagged (i.e. Markup Language-type) document.
 * Examples of a tagged document would be XML or HTML.
 */
trait Tag {

  /**
   * Method to yield the name of this Tag
   *
   * @return the name, that's to say what goes between &lt; and &gt;
   */
  def name: String

  /**
   * Method to yield the attributes of this Tag.
   *
   * @return a sequence of Attributes
   */
  def attributes: Seq[Attribute]

  /**
   * Method to yield the content of this Tag.
   *
   * @return the (optional) content as a String.
   */
  def content: Option[String]

  /**
   * Method to yield the child Tags of this Tag.
   *
   * @return a Seq of Tags.
   */
  def tags: Seq[Tag]

  /**
   * Method to add a child to this Tag
   *
   * @param tag the tag to be added
   * @return a new version of this Tag with the additional tag added as a child
   */
  def :+(tag: Tag): Tag

  /**
   * Method to yield the tag names depth-first in a Seq
   *
   * TEST
   *
   * @return a sequence of tag names
   */
  def \\ : Seq[String] = name +: (for (t <- tags; x <- t.\\) yield x)
}

/**
 * Abstract class representing a base tag.
 *
 * @param name       the name of the tag.
 * @param attributes the attributes as a sequence.
 * @param content    an optional content.
 * @param tags       the children tags.
 * @param rules      an implicit set of rules (like a DTD).
 */
abstract class BaseTag(name: String, attributes: Seq[Attribute], content: Option[String], tags: Seq[Tag])(implicit rules: TagRules) extends Tag {

  override def toString: String = s"""\n${tagString()}$contentString$tagsString${tagString(true)}"""

  private lazy val tagsString = if (tags.isEmpty) "" else tags mkString ""

  private lazy val contentString: String = content.getOrElse("")

  private def tagString(close: Boolean = false) = s"<${nameString(close)}${attributeString(close)}>"

  private def attributeString(close: Boolean) = if (close || attributes.isEmpty) "" else " " + attributes.mkString(" ")

  private def nameString(close: Boolean = false) = (if (close) "/" else "") + name
}

object Attribute {

  /**
   * TEST
   *
   * @param m a Map of attributes.
   * @return a Seq[Attribute].
   */
  def mapToAttributes(m: Map[String, String]): Seq[Attribute] = m.toSeq.map(apply)

  def apply(kv: (String, String)): Attribute = convertFromTuple(kv)

  implicit def convertFromTuple(kv: (String, String)): Attribute = Attribute(kv._1, kv._2)
}

/**
 * For future expansion.
 * The tag rules will allow us to check the model of a Tag.
 * For example, does it conform to HTML5?
 * Or XML, etc?
 */
trait TagRules
