/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.{flatspec, matchers}

class NodeSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  behavior of "NodeSpec"

  it should "mergeLeft" in {
    val node1 = Node("left leaf")
    val left = Node("left", Some("left"), Map("which" -> "left"), Seq(node1))
    val node2 = Node("right leaf")
    val right = Node("right", Some("right"), Map("which" -> "right"), Seq(node2))
    val result = Node.mergeLeft(left, right)
    println(result)
  }

  it should "mergeRight" in {
    val node1 = Node("left leaf")
    val left = Node("left", Some("left"), Map("which" -> "left"), Seq(node1))
    val node2 = Node("right leaf")
    val right = Node("right", Some("right"), Map("which" -> "right"), Seq(node2))
    val result = Node.mergeRight(left, right)
    println(result)

  }

}
