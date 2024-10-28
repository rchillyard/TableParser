/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render.tag

import scala.language.implicitConversions

/**
 * Case class to model an HTML document.
 *
 * TESTME
 *
 * @param name       the name of the tag at the root of the document.
 * @param attributes the attributes of the tag.
 * @param content    the (optional) content of the tag.
 * @param tags       the child tags.
 * @param rules      the "rules" (currently ignored) but useful in the future to validate documents.
 */
case class HTML(name: String, attributes: Seq[Attribute], content: Option[String], tags: Seq[Tag])(implicit rules: TagRules) extends BaseTag(name, attributes, content, tags) {

  /**
   * Method to add a child to this Tag
   *
   * TESTME
   *
   * @param tag the tag to be added
   * @return a new version of this Tag with the additional tag added as a child
   */
  def :+(tag: Tag): Tag = HTML(name, attributes, content, tags :+ tag)
}

/**
 * Companion object to HTML
 */
object HTML {

  implicit object HtmlRules extends TagRules

  // TESTME
  def apply(name: String, attributes: Map[String, String], content: Option[String]): HTML = apply(defaultName(name), for (attr <- attributes.toSeq) yield Attribute(attr), content, Nil)

  // TESTME
  def apply(name: String, attributes: Map[String, String]): HTML = apply(name, attributes, None)

  // TESTME
  def apply(name: String): HTML = apply(name, Map.empty, None)

  // TESTME
  def apply(name: String, content: Option[String]): HTML = apply(name, Map.empty, content)

  // TESTME
  private def defaultName(name: String): String = if (name == "") "span" else name
}

