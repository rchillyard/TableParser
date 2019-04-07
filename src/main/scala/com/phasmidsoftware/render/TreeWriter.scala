/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import scala.annotation.implicitNotFound

/**
	* This trait defines the behavior of a hierarchical writer of object.
	* For example, U might be defined as an HTML or XML document.
	* TreeWriter is of course typically used as a type class.
	*
	* CONSIDER parameterizing the underlying type of the content parameter.
	*
	* @tparam U the type of a node of the tree.
	*/
@implicitNotFound(msg = "Cannot find an implicit instance of TreeWriter[${U}].")
trait TreeWriter[U] {

	def node(tag: String, content: Option[String], attributes: Seq[String], children: Seq[U]): U

}
