package com.phasmidsoftware.parse

import com.phasmidsoftware.table.MovieParser.columnHelper
import org.scalatest.{FlatSpec, Matchers}

class ColumnHelperSpec extends FlatSpec with Matchers {

  behavior of "ColumnHelper"

  it should "convert correctly with format" in {
    val ch = columnHelper(Some("$x_$c"), "facebookLikes" -> "facebook_likes")
    ch.lookup(None, "facebookLikes") shouldBe "facebook_likes"
    ch.lookup(Some("X"), "facebookLikes") shouldBe "X_facebook_likes"
  }

  it should "convert correctly w/o format" in {
    val ch = columnHelper("facebookLikes" -> "facebook_likes")
    ch.lookup(None, "facebookLikes") shouldBe "facebook_likes"
    ch.lookup(Some("X"), "facebookLikes") shouldBe "facebook_likes"
  }

  behavior of "ColumnHelper with name mapper"

  def camelCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z0-9])", "_$1")

  it should "convert correctly with format" in {
    val ch = columnHelper(camelCaseColumnNameMapper _, Some("$x_$c"))
    ch.lookup(None, "facebookLikes") shouldBe "facebook_Likes"
    ch.lookup(Some("X"), "facebookLikes") shouldBe "X_facebook_Likes"
  }

  it should "convert correctly w/o format" in {
    val ch = columnHelper(camelCaseColumnNameMapper _)
    ch.lookup(None, "facebookLikes") shouldBe "facebook_Likes"
    ch.lookup(Some("X"), "facebookLikes") shouldBe "facebook_Likes"
  }
}
