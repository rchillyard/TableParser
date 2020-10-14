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

  /**
    * This method is required to convert a Node into a U.
    *
    * @param node the Node to be converted.
    * @return an instance of U.
    */
  def evaluate(node: Node): U

}

/**
  * This case class defines a Node in the hierarchical output produced by rendering.
  * This class is used in conjunction with TreeWriter.
  * The reason for this temporary structure is that we need the ability to merge (or otherwise process) nodes of the tree.
  * Since U (see TreeWriter) is an opaque type as far as this code is concerned, we need our own representation of the tree.
  *
  * @param style      a label that characterizes a particular node type.
  *                   This will typically be translated directly into the "tag" parameter of the corresponding U type.
  * @param content    the content of this Node, if any.
  * @param attributes the attributes of this Node (may be empty).
  * @param children   the children of this Node (may be empty).
  */
case class Node(style: String, content: Option[String], attributes: Map[String, String], children: Seq[Node]) {

  /**
    * Method to eliminate nodes of the form Node("", None, Map.empty, ...).
    *
    * @return a subtree rooted at this, but with nodes trimmed.
    */
  lazy val trim: Node = this match {
    case Node(s, wo, kVm, ns) => Node(s, wo, kVm, for (n <- ns; x <- doTrim(n)) yield x)
  }

  private def doTrim(n: Node): Seq[Node] = n match {
    case Node("", None, kVm, ns) if kVm.isEmpty => ns map (_.trim)
    case _ => Seq(n.trim)
  }
}

/**
  * Companion object to Node.
  */
object Node {

  /**
    * Create a content-less leaf Node (with no children).
    * NOTE: I'm not sure if this makes sense and is only used by unit tests.
    *
    * @param style      a label that characterizes a particular node type.
    *                   This will typically be translated directly into the "tag" parameter of the corresponding U type.
    * @param attributes the attributes of this Node (may be empty).
    * @return a new Node.
    */
  def apply(style: String, attributes: Map[String, String]): Node = apply(style, None, attributes)

  /**
    * Create a leaf Node (with no children).
    *
    * @param style      a label that characterizes a particular node type.
    *                   This will typically be translated directly into the "tag" parameter of the corresponding U type.
    * @param content    the content of this Node, if any.
    * @param attributes the attributes of this Node (may be empty).
    * @return a new Node.
    */
  def apply(style: String, content: Option[String], attributes: Map[String, String]): Node = apply(style, content, attributes, Nil)

  /**
    * Create a content-less, no-attribute, leaf Node (with no children).
    * NOTE: I'm not sure if this makes sense and is only used by unit tests.
    *
    * @param style a label that characterizes a particular node type.
    *              This will typically be translated directly into the "tag" parameter of the corresponding U type.
    * @return a new Node.
    */
  def apply(style: String): Node = apply(style, Nil)

  /**
    * Create a Node with only style and children.
    *
    * @param style    a label that characterizes a particular node type.
    *                 This will typically be translated directly into the "tag" parameter of the corresponding U type.
    * @param children the children of this Node (may be empty).
    * @return a new Node.
    */
  def apply(style: String, children: Seq[Node]): Node = apply(style, Map[String, String](), children)

  /**
    * Create a Node with no content.
    *
    * @param style      a label that characterizes a particular node type.
    *                   This will typically be translated directly into the "tag" parameter of the corresponding U type.
    * @param attributes the attributes of this Node (may be empty).
    * @param children   the children of this Node (may be empty).
    * @return a new Node.
    */
  def apply(style: String, attributes: Map[String, String], children: Seq[Node]): Node = apply(style, None, attributes, children)

  /**
    * TODO can eliminate
    *
    * Merge two Nodes together, taking the style, content (if any) and attributes from the left node.
    *
    * @param left  the left Node (its children will precede the children of the right Node).
    * @param right the right Node (its children will succeed the children of the left Node).
    * @return a new Node containing all the children of the left and right.
    */
  def mergeLeft(left: Node, right: Node): Node = Node(left.style, left.content, left.attributes, left.children ++ right.children)

  /**
    * TODO can eliminate
    *
    * Merge two Nodes together, taking the style, content (if any) and attributes from the right node.
    *
    * @param left  the left Node (its children will precede the children of the right Node).
    * @param right the right Node (its children will succeed the children of the left Node).
    * @return a new Node containing all the children of the left and right.
    */
  def mergeRight(left: Node, right: Node): Node = Node(right.style, right.content, right.attributes, left.children ++ right.children)

}

