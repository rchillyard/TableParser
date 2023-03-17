package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.parallel.CollectionConverters._

class ContentSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Content"

  it should "drop" in {

  }

  it should "toIndexedSeq" in {

  }

  it should "takeWhile" in {

  }

  it should "filterNot" in {

  }

  it should "head" in {

  }

  it should "knownSize" in {

  }

  it should "foreach" in {

  }

  it should "iterator" in {

  }

  it should "foldLeft" in {

  }

  it should "slice" in {

  }

  it should "toArray" in {

  }

  it should "map" in {

  }

  it should "$plus$plus" in {

  }

  it should "dropWhile" in {

  }

  it should "filter" in {

  }

  it should "flatMap" in {

  }

  it should "take" in {

  }

  it should "toSeq" in {

  }

  it should "size" in {

  }

  it should "apply1" in {
    val target: Content[Int] = Content(List(1, 2, 3))
    target.toSeq shouldBe List(1, 2, 3)
  }

  it should "apply2" in {
    val target: Content[Int] = Content(List(1, 2, 3).par)
    target.toSeq shouldBe List(1, 2, 3)
  }

}
