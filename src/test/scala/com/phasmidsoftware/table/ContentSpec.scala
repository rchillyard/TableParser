package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.parallel.CollectionConverters._

/**
 * ContentSpec
 *
 * NOTE: The methods here depend on the behavior of the ParIterable parameter of Content.
 */
class ContentSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Content"

  it should "ordered" in {
    val target: Content[Int] = Content(List(1, 2, 3))
    target.ordered shouldBe List(1, 2, 3)
  }

  it should "sorted" in {
    val target: Content[Int] = Content(List(2, 1, 3))
    target.sorted shouldBe Content(List(1, 2, 3))
  }

  it should "drop" in {
    val target: Content[Int] = Content(List(1, 2, 3))
    // CONSIDER forcing sorted on the drop method.
    target.drop(1).toSeq shouldBe List(2, 3)
  }

  it should "take" in {
    val target: Content[Int] = Content(List(1, 2, 3))
    target.take(2).toSeq shouldBe List(1, 2)
  }

  it should "slice" in {
    val target: Content[Int] = Content(List(1, 2, 3, 4))
    target.slice(1, 3).toSeq shouldBe List(2, 3)
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
