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
	* @tparam U the type of a node of the tree.
	*/
@implicitNotFound(msg = "Cannot find an implicit instance of TreeWriter[${U}].")
trait TreeWriter[U] {

	/**
		* Create a new sub-tree with children defined by the sequence us.
		*
		* @param us a sequence of instances of U.
		* @param ao an optional attribute name.
		* @return a new instance of U (i.e. a node).
		*/
	def subtree(us: Seq[U], ao: Option[String]): U

	/**
		* Create a new node which may be a sub-tree or a leaf, depending on the nature of type T
		*
		* @param t  an instance of T.
		* @param ao an optional attribute name.
		* @tparam T the input type that is to be rendered as a node.
		* @return a new instance of U (i.e. a node).
		*/
	def node[T: Renderer](t: T, ao: Option[String]): U

	/**
		* Create a new leaf node.
		* TODO figure out whether t should be an Any or a T.
		*
		* @param t  an instance of T.
		* @param ao an optional attribute name.
		* @return a new instance of U (i.e. a node).
		*/
	def leaf(t: Any, ao: Option[String]): U
}
