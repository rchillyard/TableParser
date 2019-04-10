/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import scala.annotation.implicitNotFound

/**
	* This trait defines the behavior of a hierarchical writer of objects.
	* For example, U might be defined as an HTML or XML document.
	* TreeWriter is of course typically used as a type class.
	*
	* CONSIDER parameterizing the underlying type of the content parameter.
	*
	* @tparam U the type of a node of the tree.
	*/
@implicitNotFound(msg = "Cannot find an implicit instance of TreeWriter[${U}].")
trait TreeWriter[U] {

	def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[U]): U

	def node(tag: String, attributes: Map[String, String], children: Seq[U]): U = node(tag, None, attributes, children)

	def node(tag: String, children: Seq[U]): U = node(tag, Map[String, String](), children)

	def node(tag: String, content: Option[String], attributes: Map[String, String]): U = node(tag, content, attributes, Nil)

	def node(tag: String, attributes: Map[String, String]): U = node(tag, None, attributes)

	def node(tag: String): U = node(tag, Nil)

	def addChild(parent: U, child: U): U
}

