/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.flatspec
import org.scalatest.matchers.should

class WritableSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "Writable"

  implicit object StringBuilderWriteable extends Writable[StringBuilder] {
    override def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)

    override def unit: StringBuilder = new StringBuilder

    override def delimiter: CharSequence = "|"
  }

  it should "write value" in {
    val sw = implicitly[Writable[StringBuilder]]
    val o = sw.unit
    sw.writeValue(o)(1)
    o.toString shouldBe "1"
  }

  it should "write value containing delimiter" in {
    val sw = implicitly[Writable[StringBuilder]]
    val o = sw.unit
    sw.writeValue(o)("a|b")
    o.toString shouldBe "\"a|b\""
  }

  it should "write value containing quote" in {
    val sw = implicitly[Writable[StringBuilder]]
    val o = sw.unit
    sw.writeValue(o)("""a"b""")
    o.toString shouldBe "\"a\"\"b\""
  }

  it should "writeRowElements" in {
    val sw = implicitly[Writable[StringBuilder]]
    val o = sw.unit
    sw.writeRowElements(o)(Seq(1, 2))
    o.toString shouldBe "1|2"
  }

  case class Complex(r: Double, i: Double)

  it should "writeRow" in {
    val sw = implicitly[Writable[StringBuilder]]
    val o = sw.unit
    sw.writeRow(o)(Complex(1, -1))
    o.toString shouldBe
      """1.0|-1.0
        				|""".stripMargin
  }

}
