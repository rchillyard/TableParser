package com.phasmidsoftware.tableparser.core.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Random

class TopNSpec extends AnyFlatSpec with should.Matchers:

  behavior of "TopN.greatest"

  it should "keep the n greatest values, greatest first" in:
    (TopN.greatest[Int](3) ++ Seq(5, 1, 9, 3, 7, 2)).toSeq shouldBe Seq(9, 7, 5)

  it should "keep every value when there are fewer than n" in:
    (TopN.greatest[Int](10) ++ Seq(2, 3, 1)).toSeq shouldBe Seq(3, 2, 1)

  it should "keep nothing when n is 0" in:
    (TopN.greatest[Int](0) ++ Seq(2, 3, 1)).toSeq shouldBe Nil

  it should "keep duplicates" in:
    (TopN.greatest[Int](3) ++ Seq(4, 4, 1, 4, 4)).toSeq shouldBe Seq(4, 4, 4)

  it should "hold no more than n values" in:
    val t = TopN.greatest[Int](5) ++ (1 to 100000)
    t.size shouldBe 5
    t.toSeq shouldBe Seq(100000, 99999, 99998, 99997, 99996)

  it should "agree with sorting" in:
    val random = new Random(0)
    val xs = Seq.fill(1000)(random.nextInt(100))
    (TopN.greatest[Int](17) ++ xs).toSeq shouldBe xs.sorted.reverse.take(17)

  it should "use the given Ordering" in:
    given Ordering[String] = Ordering.by(_.length)
    (TopN.greatest[String](2) ++ Seq("a", "abc", "ab", "abcd")).toSeq shouldBe Seq("abcd", "abc")

  it should "not allow a negative n" in:
    an[IllegalArgumentException] should be thrownBy TopN.greatest[Int](-1)

  it should "be immutable" in:
    val t = TopN.greatest[Int](2) ++ Seq(1, 2)
    val u = t :+ 3
    t.toSeq shouldBe Seq(2, 1)
    u.toSeq shouldBe Seq(3, 2)

  behavior of "TopN.least"

  it should "keep the n least values, least first" in:
    (TopN.least[Int](3) ++ Seq(5, 1, 9, 3, 7, 2)).toSeq shouldBe Seq(1, 2, 3)

  it should "agree with sorting" in:
    val random = new Random(1)
    val xs = Seq.fill(1000)(random.nextInt(100))
    (TopN.least[Int](17) ++ xs).toSeq shouldBe xs.sorted.take(17)

  it should "not allow a negative n" in:
    an[IllegalArgumentException] should be thrownBy TopN.least[Int](-1)
