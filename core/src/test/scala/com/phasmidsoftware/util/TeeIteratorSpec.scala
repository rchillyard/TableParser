package com.phasmidsoftware.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TeeIteratorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "TeeIterator"

  it should "next 0" in {
    val iterator = List(1, 2, 3).iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](0)(iterator)
    teeIterator.hasNext shouldBe true
    teeIterator.next() shouldBe 1
    teeIterator.hasNext shouldBe true
    teeIterator.next() shouldBe 2
    teeIterator.hasNext shouldBe true
    teeIterator.next() shouldBe 3
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Nil
  }

  it should "next 1" in {
    val iterator = List(1, 2, 3).iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](1)(iterator)
    teeIterator.hasNext shouldBe true
    teeIterator.next() shouldBe 2
    teeIterator.hasNext shouldBe true
    teeIterator.next() shouldBe 3
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Seq(1)
  }

  it should "next 2" in {
    val iterator = List(1, 2, 3).iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](2)(iterator)
    teeIterator.hasNext shouldBe true
    teeIterator.next() shouldBe 3
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Seq(1, 2)
  }

  it should "next 3" in {
    val iterator = List(1, 2, 3).iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](3)(iterator)
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Seq(1, 2, 3)
  }

  it should "next 4" in {
    val iterator = List(1, 2, 3).iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](4)(iterator)
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Seq(1, 2, 3)
  }

  it should "next empty" in {
    val iterator = List().iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](0)(iterator)
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Nil
  }

  it should "next -1" in {
    val iterator = List().iterator
    val teeIterator: TeeIterator[Int] = new TeeIterator[Int](-1)(iterator)
    teeIterator.hasNext shouldBe false
    teeIterator.tee shouldBe Nil
  }

}
