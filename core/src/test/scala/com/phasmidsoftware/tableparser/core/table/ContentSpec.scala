package com.phasmidsoftware.tableparser.core.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.parallel.CollectionConverters._

/**
 * ContentSpec
 *
 * NOTE: as of 2.0, Content keeps its rows in order (in a Vector); see "par" for parallel processing.
 */
class ContentSpec extends AnyFlatSpec with should.Matchers:

  behavior of "Content"

  it should "ordered" in:
    val target: Content[Int] = Content(List(1, 2, 3))
    target.sorted.toSeq shouldBe List(1, 2, 3)

  it should "sorted" in:
    val target: Content[Int] = Content(List(2, 1, 3))
    target.sorted shouldBe Content(List(1, 2, 3))

  it should "drop" in:
    val target: Content[Int] = Content(List(1, 2, 3))
    // CONSIDER forcing sorted on the drop method.
    target.drop(1).toSeq shouldBe List(2, 3)

  it should "take" in:
    val target: Content[Int] = Content(List(1, 2, 3))
    target.take(2).toSeq shouldBe List(1, 2)

  it should "slice" in:
    val target: Content[Int] = Content(List(1, 2, 3, 4))
    target.slice(1, 3).toSeq shouldBe List(2, 3)

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

  it should "apply1" in:
    val target: Content[Int] = Content(List(1, 2, 3))
    target.toSeq shouldBe List(1, 2, 3)

  it should "apply2" in:
    val target: Content[Int] = Content(List(1, 2, 3).par)
    target.toSeq shouldBe List(1, 2, 3)

  it should "keep its rows in order (2.0)" in:
    val target: Content[Int] = Content(1 to 1000)
    (target map (_ * 2) filter (_ % 3 != 0)).toSeq shouldBe (1 to 1000).map(_ * 2).filter(_ % 3 != 0)
    target.iterator.toSeq shouldBe (1 to 1000)

  it should "give its rows as a parallel collection with par" in:
    val target: Content[Int] = Content(1 to 1000)
    target.par.map(_ * 2).sum shouldBe (1 to 1000).map(_ * 2).sum
