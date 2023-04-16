package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class SequentialSpec extends AnyFlatSpec with should.Matchers {

  case class Tester(x: Int)

  object Tester {
    val negOne: Tester = Tester(-1)
    val zero: Tester = Tester(0)
    val one: Tester = Tester(1)
  }

  case class TesterOpt(x: Option[Int])

  object TesterOpt {
    val negOne: TesterOpt = TesterOpt(Some(-1))
    val zero: TesterOpt = TesterOpt(Some(0))
    val one: TesterOpt = TesterOpt(Some(1))
    val none: TesterOpt = TesterOpt(None)
  }

  case class TesterTry(x: Try[Int])

  object TesterTry {
    val negOne: TesterTry = TesterTry(Success(-1))
    val zero: TesterTry = TesterTry(Success(0))
    val one: TesterTry = TesterTry(Success(1))
    val failure: TesterTry = TesterTry(Failure(new NoSuchElementException))
  }

  case class TesterStringOpt(x: Option[String])

  object TesterStringOpt {
    val a: TesterStringOpt = TesterStringOpt(Some("a"))
    val b: TesterStringOpt = TesterStringOpt(Some("b"))
    val c: TesterStringOpt = TesterStringOpt(Some("c"))
    val none: TesterStringOpt = TesterStringOpt(None)
  }

  behavior of "NonSequential"

  it should "ordering" in {
    val ordering = NonSequential.ordering[Tester, Int](_.x)
    ordering.compare(Tester.zero, Tester.one) shouldBe -1
    ordering.compare(Tester.zero, Tester.zero) shouldBe 0
    ordering.compare(Tester.one, Tester.zero) shouldBe 1
    ordering.compare(Tester.negOne, Tester.zero) shouldBe -1
    ordering.compare(Tester.one, Tester.one) shouldBe 0
    ordering.compare(Tester.one, Tester.negOne) shouldBe 1
  }

  it should "optionalOrdering" in {
    val ordering = NonSequential.optionalOrdering[TesterOpt, Int](_.x)
    ordering.compare(TesterOpt.zero, TesterOpt.one) shouldBe -1
    ordering.compare(TesterOpt.zero, TesterOpt.zero) shouldBe 0
    ordering.compare(TesterOpt.one, TesterOpt.zero) shouldBe 1
    ordering.compare(TesterOpt.negOne, TesterOpt.zero) shouldBe -1
    ordering.compare(TesterOpt.one, TesterOpt.one) shouldBe 0
    ordering.compare(TesterOpt.one, TesterOpt.negOne) shouldBe 1
    ordering.compare(TesterOpt.none, TesterOpt.one) shouldBe -1
    ordering.compare(TesterOpt.none, TesterOpt.none) shouldBe 0
    ordering.compare(TesterOpt.negOne, TesterOpt.none) shouldBe -1
  }

  it should "optionalStringOrdering" in {
    val ordering = NonSequential.optionalOrdering[TesterStringOpt, String](_.x)
    ordering.compare(TesterStringOpt.b, TesterStringOpt.c) shouldBe -1
    ordering.compare(TesterStringOpt.b, TesterStringOpt.b) shouldBe 0
    ordering.compare(TesterStringOpt.c, TesterStringOpt.b) shouldBe 1
    ordering.compare(TesterStringOpt.a, TesterStringOpt.b) shouldBe -1
    ordering.compare(TesterStringOpt.c, TesterStringOpt.c) shouldBe 0
    ordering.compare(TesterStringOpt.c, TesterStringOpt.a) shouldBe 2
    ordering.compare(TesterStringOpt.none, TesterStringOpt.c) shouldBe -1
    ordering.compare(TesterStringOpt.none, TesterStringOpt.none) shouldBe 0
    ordering.compare(TesterStringOpt.a, TesterStringOpt.none) shouldBe 1
  }

  it should "tryOrdering" in {
    val ordering = NonSequential.tryOrdering[TesterTry, Int](_.x)
    ordering.compare(TesterTry.zero, TesterTry.one) shouldBe -1
    ordering.compare(TesterTry.zero, TesterTry.zero) shouldBe 0
    ordering.compare(TesterTry.one, TesterTry.zero) shouldBe 1
    ordering.compare(TesterTry.negOne, TesterTry.zero) shouldBe -1
    ordering.compare(TesterTry.one, TesterTry.one) shouldBe 0
    ordering.compare(TesterTry.one, TesterTry.negOne) shouldBe 1
    ordering.compare(TesterTry.failure, TesterTry.one) shouldBe -1
    ordering.compare(TesterTry.failure, TesterTry.failure) shouldBe 0
    ordering.compare(TesterTry.negOne, TesterTry.failure) shouldBe -1
  }
}
