package com.phasmidsoftware.csvparser

import org.scalatest.{FlatSpec, Matchers}

class RowSpec extends FlatSpec with Matchers {

  behavior of "TupleRow"
  it should "construct a new TupleRow" in {

    import scala.reflect._
    val r: TupleRow = new TupleRow(Seq(classTag[Int]), Seq("A")) {

      override def productElement(n: Int): Any = if (n == 0) 1 else throw RowException(s"$n is out of range")

      override def productArity: Int = 1

      override def canEqual(that: Any): Boolean = false
    }
    r.getCell[Int]("A") shouldBe Some(1)
  }


  behavior of "Row1"
  it should "create" in {
    val r = Row1.create(1, "A")
    r.getCell[Int]("A") shouldBe Some(1)
  }
  it should "apply" in {
    val x = SeqRow.create(Seq("1"), "A")
    val r = Row1(x) { case s: String => s.toInt }
    r.getCell[Int]("A") shouldBe Some(1)
  }

  behavior of "Row2"
  it should "create" in {
    val r = Row2.create(1, 2, "A", "B")
    r.getCell[Int]("A") shouldBe Some(1)
    r.getCell[Int]("B") shouldBe Some(2)
    r.asTuple should matchPattern {
      case (1, 2) =>
    }
  }
  it should "apply" in {
    val x = SeqRow.create(Seq("1", "Hello"), "A", "B")
    val r = Row2(x)({ case s: String => s.toInt }, { case s: String => s })
    r.getCell[Int]("A") shouldBe Some(1)
    r.getCell[String]("B") shouldBe Some("Hello")
  }

  behavior of "Row3"
  it should "create" in {
    val r = Row3.create(1, 2, "Junk", "A", "B", "C")
    r.getCell[Int]("A") shouldBe Some(1)
    r.getCell[Int]("B") shouldBe Some(2)
    r.getCell[String]("C") shouldBe Some("Junk")
    r.asTuple should matchPattern {
      case (1, 2, "Junk") =>
    }
  }

  it should "apply" in {
    val x = SeqRow.create(Seq("1", "Hello", "true"), "A", "B", "C")
    val r = Row3(x)({ case s: String => s.toInt }, { case s: String => s }, { case s: String => s.toBoolean })
    println(r)
    r.getCell[Int]("A") shouldBe Some(1)
    r.getCell[String]("B") shouldBe Some("Hello")
    r.getCell[Boolean]("C") shouldBe Some(true)
  }

  behavior of "SeqRow"
  it should "implement productElement and getCell" in {
    val r = SeqRow.create(Seq(1, 2, 3), "A", "B", "C")
    r.productElement(0) shouldBe 1
    r.getCell(0) shouldBe Some(1)
    r.getCell("A") shouldBe Some(1)
    r.productElement(1) shouldBe 2
    r.getCell(1) shouldBe Some(2)
    r.getCell("B") shouldBe Some(2)
    r.productElement(2) shouldBe 3
    r.getCell(2) shouldBe Some(3)
    r.getCell("C") shouldBe Some(3)
  }

  it should "implement map" in {
    val p = SeqRow.create(Seq("1", "2", "3"), "A", "B", "C")
    val r = p map (_.toInt)
    r.productElement(0) shouldBe 1
    r.getCell(0) shouldBe Some(1)
    r.getCell("A") shouldBe Some(1)
    r.productElement(1) shouldBe 2
    r.getCell(1) shouldBe Some(2)
    r.getCell("B") shouldBe Some(2)
    r.productElement(2) shouldBe 3
    r.getCell(2) shouldBe Some(3)
    r.getCell("C") shouldBe Some(3)
  }

  it should "implement ++" in {
    val p = SeqRow.create(Seq(1, 2, 3), "A", "B", "C")
    val r = SeqRow.create(Seq(4, 5, 6), "D", "E", "F")
    val s = p ++ r
    s.getCell(0) shouldBe Some(1)
    s.getCell("A") shouldBe Some(1)
    s.getCell("B") shouldBe Some(2)
    s.productElement(5) shouldBe 6
    s.getCell(5) shouldBe Some(6)
    s.getCell("F") shouldBe Some(6)
  }

  it should "implement flatMap" in {
    val p = SeqRow.create(Seq(1, 2), "A", "B")
    val s = p flatMap (x => SeqRow(Seq(x, x * 3))(Seq("A" + x, "B" + x)))
    s.getCell(0) shouldBe Some(1)
    s.getCell("A1") shouldBe Some(1)
    s.getCell("B1") shouldBe Some(3)
    s.productElement(3) shouldBe 6
    s.getCell(3) shouldBe Some(6)
    s.getCell("B2") shouldBe Some(6)
  }
}
