/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.write

import com.phasmidsoftware.write.WritableSpec.complexFile
import java.io.{File, FileWriter}
import org.scalatest.flatspec
import org.scalatest.matchers.should

class WritableSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "Writable"

  private val sw = Writable.stringBuilderWritable("|")

  it should "write value" in {
    val o = sw.unit
    sw.writeValue(o)(1)
    o.toString shouldBe "1"
  }

  it should "write value containing delimiter" in {
    val o = sw.unit
    sw.writeValue(o)("a|b")
    o.toString shouldBe "\"a|b\""
  }

  it should "write value containing quote" in {
    val o = sw.unit
    sw.writeValue(o)("""a"b""")
    o.toString shouldBe "\"a\"\"b\""
  }

  it should "writeRowElements" in {
    val o = sw.unit
    sw.writeRowElements(o)(Seq(1, 2))
    o.toString shouldBe "1|2"
  }

  case class Complex(r: Double, i: Double)

  it should "writeRow" in {
    val o = sw.unit
    sw.writeRow(o)(Complex(1, -1))
    o.toString shouldBe
            """1.0|-1.0
        				|""".stripMargin
  }

  it should "writeRowToFile" in {
    val file = new File(complexFile)
    val fw: Writable[FileWriter] = Writable.fileWritable(file)
    val o = fw.unit
    fw.writeRow(o)(Complex(1, -1))
    fw.close(o)
  }

  it should "writeRawLine" in {
    val sw = Writable.stringBuilderWritable()
    val o = sw.unit
    sw.writeRawLine(o)("Hello World!")
    sw.close(o)
    o.toString shouldBe "Hello World!\n"
  }

}

object WritableSpec {
  val complexFile = "tmp/Writable-writeRowToFile.csv"
}