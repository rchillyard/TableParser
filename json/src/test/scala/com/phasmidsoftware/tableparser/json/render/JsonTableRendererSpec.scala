package com.phasmidsoftware.tableparser.json.render

import com.phasmidsoftware.tableparser.core.parse.{CellParser, TableParserHelper}
import com.phasmidsoftware.tableparser.core.render.{Renderer, TableRenderable}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import com.phasmidsoftware.tableparser.json.table.TableCodecs.given
import io.circe.Codec
import io.circe.parser.decode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Try

class JsonTableRendererSpec extends AnyFlatSpec with should.Matchers:

  import JsonTableRendererSpec._

  given Renderer[Table[Partnership], String] = new JsonTableRenderer[Partnership]

  behavior of "JsonTableRenderer"

  it should "render Partnership table" in:
    val strings = List("First,Last", "Abel,Sumac", "Alma,Alder", "Anya,Pine", "Beatrix,Gorse")
    val wy: Try[String] = for (pt <- Table.parse[Table[Player]](strings); q = Player.convertTable(pt); w = q.asInstanceOf[TableRenderable[Partnership]].render) yield w
    matchTry(wy):
      case p =>
        p shouldBe partnershipsJson("playerA", "playerB")
        decode[Table[Partnership]](p) should matchPattern { case Right(HeadedTable(_, _)) => }

  it should "read back what it renders" in:
    val strings = List("First,Last", "Abel,Sumac", "Alma,Alder")
    val pty: Try[Table[Partnership]] = for (pt <- Table.parse[Table[Player]](strings)) yield Player.convertTable(pt)
    matchTry(pty):
      case pt =>
        val decoded = decode[Table[Partnership]](summon[Renderer[Table[Partnership], String]].render(pt))
        decoded.map(_.toList) shouldBe Right(List(Partnership("Abel S", "Alma A")))
        decoded.map(_.maybeHeader) shouldBe Right(Some(Header(Seq("playerA", "playerB"), Nil)))

  it should "read a table without a header" in:
    decode[Table[Partnership]]("""{"rows": [{"playerA": "Abel S", "playerB": "Alma A"}]}""").map(_.maybeHeader) shouldBe Right(None)

  it should "render and read a table with no rows" in:
    val table: Table[Partnership] = Table(Nil, None)
    val w = summon[Renderer[Table[Partnership], String]].render(table)
    w shouldBe "{\n  \"rows\" : [\n  ]\n}"
    decode[Table[Partnership]](w).map(_.size) shouldBe Right(0)

  /**
   * Same as "support fixed header and write to Json" in 1.x's TableParserHelperSpec: the input has no header row.
   */
  it should "render a table whose header is fixed" in:
    val strings = List("Abel,Sumac", "Alma,Alder", "Anya,Pine", "Beatrix,Gorse")
    val pty: Try[Table[HeadlessPlayer]] = Table.parse[Table[HeadlessPlayer]](strings.iterator)
    matchTry(pty):
      case pt@HeadedTable(_, _) =>
        val sht: Table[Partnership] = HeadlessPlayer.convertTable(pt)
        summon[Renderer[Table[Partnership], String]].render(sht) shouldBe partnershipsJson("first", "last")

object JsonTableRendererSpec:

  /**
   * The expected JSON for the two partnerships of Abel, Alma, Anya and Beatrix.
   *
   * @param a the name of the first column.
   * @param b the name of the second column.
   * @return a JSON String.
   */
  def partnershipsJson(a: String, b: String): String =
    s"""{
       |  "rows" : [
       |    {
       |      "playerA" : "Abel S",
       |      "playerB" : "Alma A"
       |    },
       |    {
       |      "playerA" : "Anya P",
       |      "playerB" : "Beatrix G"
       |    }
       |  ],
       |  "header" : [
       |    "$a",
       |    "$b"
       |  ]
       |}""".stripMargin

  case class Player(first: String, last: String):
    def nickname: String = s"$first ${last.head}"

  object Player extends TableParserHelper[Player]():
    def cellParser: CellParser[Player] = CellParser.derived[Player]

    /**
     * Method to transform a Table[Player] into a Table[Partnership].
     *
     * The requirements of the application are that the rows of the Player table are grouped by twos
     * and each resulting entity (an array of length 2) is taken to form a Partnership.
     *
     * CONSIDER merging with duplicate code in: TableParserSpec.scala
     *
     * @param pt a Table[Player]
     * @return a Table[Partnership]
     */
    def convertTable(pt: Table[Player]): Table[Partnership] =
      pt.processRows(xs => Content((xs.toSeq grouped 2).map(r => Partnership(r.head.nickname, r.last.nickname)).toList)).replaceHeader(Some(Header.create("playerA", "playerB")))

  /**
   * A Player read from input which has no header row (TableParserHelper's header is then fixed: first, last).
   */
  case class HeadlessPlayer(first: String, last: String):
    def nickname: String = s"$first ${last.head}"

  object HeadlessPlayer extends TableParserHelper[HeadlessPlayer](false):
    lazy val cellParser: CellParser[HeadlessPlayer] = CellParser.derived[HeadlessPlayer]

    def convertTable(pt: Table[HeadlessPlayer]): Table[Partnership] =
      pt.processRows(xs => Content((xs.toSeq grouped 2).map(r => Partnership(r.head.nickname, r.last.nickname)).toList))

  case class Partnership(playerA: String, playerB: String) derives Codec.AsObject
