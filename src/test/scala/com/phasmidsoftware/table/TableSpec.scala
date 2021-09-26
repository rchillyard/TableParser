/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.parse._
import com.phasmidsoftware.render._
import com.phasmidsoftware.util.FP.safeResource
import org.scalatest.flatspec
import org.scalatest.matchers.should

import java.io.{File, InputStream}
import java.net.URL
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class TableSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class IntPair(a: Int, b: Int) {
    def map(f: Int => Int): IntPair = IntPair(f(a), f(b))
  }

  object IntPair {

    class IntPairParser extends JavaTokenParsers {
      lazy val pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    val intPairParser = new IntPairParser

    trait IntPairRowParser extends StringParser[IntPair] {
      def parse(indexedString: (String, Int))(header: Header): Try[IntPair] = intPairParser.parseAll(intPairParser.pair, indexedString._1) match {
        case intPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse ${indexedString._1}"))
      }

      //noinspection NotImplementedCode
      def parseHeader(w: String): Try[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      type Row = IntPair

      val maybeFixedHeader: Option[Header] = Some(Header.create("a", "b"))


      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, Header[IntPair]())

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "Table"

  it should "parse from Seq[String]" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from Iterator[String]" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99").iterator)
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from Source" in {
    import IntPair._

    val source = Source.fromChars(Array('1', ' ', '2', '\n', '4', '2', ' ', '9', '9', '\n'))
    val iIty = Table.parse(source)
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from File" in {
    import IntPair._

    val iIty = Table.parseFile(new File("src/test/resources/com/phasmidsoftware/table/intPairs.csv"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from null File" in {
    import IntPair._

    val f: String = null
    val iIty = Table.parseFile(new File(f))
    iIty should matchPattern { case Failure(_) => }
  }

  it should "parse from Resource" in {
    import IntPair._

    val iIty = Table.parseResource("intPairs.csv", classOf[TableSpec])
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  behavior of "parse with safeResource"

  it should "return success for intPairs.csv" in {
    import IntPair._

    lazy val i: InputStream = classOf[TableSpec].getResourceAsStream("intPairs.csv")
    val iIty = Table.parseInputStream(i)
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "return failure(0)" in {
    import IntPair._

    val iIty = Table.parse(Source.fromResource(null))
    iIty should matchPattern { case Failure(_) => }
    iIty.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return failure(1)" in {
    import IntPair._

    lazy val i: InputStream = classOf[TableSpec].getResourceAsStream(null)
    val iIty = Table.parseInputStream(i)
    iIty should matchPattern { case Failure(_) => }
    iIty.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return failure(2)" in {
    lazy val i: InputStream = getClass.getResourceAsStream("emptyResource.txt")
    val wy = safeResource(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NoSuchElementException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return success for intPairs.csv URL with encoding" in {
    import IntPair._

    lazy val u: URL = classOf[TableSpec].getResource("intPairs.csv")
    val iIty = Table.parseResource(u, "UTF-8")
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }


  behavior of "other"

  it should "do iterator" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    val x = iIty.get.iterator
    x.hasNext shouldBe true
    x.next() shouldBe IntPair(1, 2)
    x.hasNext shouldBe true
    x.next() shouldBe IntPair(42, 99)
    x.hasNext shouldBe false
  }

  it should "map" in {
    val f: IntPair => IntPair = _ map (_ * 2)

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.map(f).rows shouldBe Seq(IntPair(2, 4), IntPair(84, 198))
  }

  it should "flatMap" in {
    val f: IntPair => Table[IntPair] = p => HeadedTable(Seq(p), Header())

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.flatMap(f).rows shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
  }

  case class HTML(x: String, ao: Option[String], attr: Map[String, String], hs: Seq[HTML])

  object HTML {
    def apply(x: String): HTML = apply(x, None, Map.empty, Nil)

    def apply(x: String, a: String): HTML = apply(x, Some(a), Map.empty, Nil)

    def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Map.empty, hs)

  }

  object IntPairHTML extends HierarchicalRenderers {

    trait HTMLTreeWriter extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.content map identity, node.attributes, node.children map evaluate)
    }

    implicit object HTMLTreeWriter extends HTMLTreeWriter

    implicit val intPairRenderer: HierarchicalRenderer[IntPair] = renderer2("IntPair")(IntPair.apply)
    implicit val r: HierarchicalRenderer[Indexed[IntPair]] = indexedRenderer("", "th")

  }

  it should "render the parsed table to CSV" in {
    import IntPair._
    val iIty: Try[Table[IntPair]] = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }

    implicit object StringBuilderWritable extends Writable[StringBuilder] {
      def unit: StringBuilder = new StringBuilder

      override def delimiter: CharSequence = "|"

      def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
    }

    implicit object DummyRenderer$$ extends Renderer[Table[IntPair], String] {
      def render(t: Table[IntPair], attrs: Map[String, String]): String = t match {
        case t: RenderableTable[IntPair] => t.RenderToWritable(StringBuilderWritable).toString
        case _ => throw TableException("render problem")
      }
    }

    val sy = iIty map {
      case r: Table[IntPair] => implicitly[Renderer[Table[IntPair], String]].render(r)
      case _ => fail("cannot render table")
    }
    sy should matchPattern { case Success(_) => }
    sy.get shouldBe "a|b\n1|2\n42|99\n"
  }

  //  it should "render the parsed table with TreeWriter" in {
  //    import IntPair._
  //    val iIty: Try[Table[IntPair]] = Table.parse(Seq("1 2", "42 99"))
  //    import IntPairHTML._
  //
  //    val hy = iIty map (_.render("table", Map()))
  //    hy should matchPattern { case Success(_) => }
  //    // CONSIDER why do we use ArrayBuffer here instead of List?
  //    hy.get shouldBe HTML("table", None, Map(), List(HTML("thead", None, Map(), List(HTML("tr", None, Map(), Seq(HTML("th", Some("a"), Map(), List()), HTML("th", Some("b"), Map(), List()))))), HTML("tbody", None, Map(), List(HTML("IntPair", None, Map(), List(HTML("", Some("1"), Map("name" -> "a"), List()), HTML("", Some("2"), Map("name" -> "b"), List()))), HTML("IntPair", None, Map(), List(HTML("", Some("42"), Map("name" -> "a"), List()), HTML("", Some("99"), Map("name" -> "b"), List())))))))
  //  }
  //
  //  def mapTo[T, U](ty: Try[T]): Try[U] = ty match {
  //    case Success(t) => Success(t.asInstanceOf[U])
  //    case Failure(x) => Failure(x)
  //  }

  behavior of "Header"

  import scala.language.postfixOps

  it should "create with letters" in {
    val header = Header(letters = true, 62)
    header.xs.head shouldBe "A"
    header.xs.last shouldBe "BJ"
  }

  it should "generateNumbers" in {
    val xs: List[String] = Header.generateNumbers take 10 toList

    xs shouldBe Seq("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  }

  it should "prepend" in {
    val xs = Header.prepend("x", Header.alphabet.to(LazyList)).take(100).toList
    xs shouldBe Seq("xA", "xB", "xC", "xD", "xE", "xF", "xG", "xH", "xI", "xJ", "xK", "xL", "xM", "xN", "xO", "xP", "xQ", "xR", "xS", "xT", "xU", "xV", "xW", "xX", "xY", "xZ")
  }

  it should "multiply" in {
    val xs = Header.multiply(List("A", "B"), Header.alphabet.to(LazyList))
    xs shouldBe Seq("AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AI", "AJ", "AK", "AL", "AM", "AN", "AO", "AP", "AQ", "AR", "AS", "AT", "AU", "AV", "AW", "AX", "AY", "AZ", "BA", "BB", "BC", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BK", "BL", "BM", "BN", "BO", "BP", "BQ", "BR", "BS", "BT", "BU", "BV", "BW", "BX", "BY", "BZ")
  }

  it should "generateLetters" in {
    val xs: List[String] = Header.generateLetters take 100 toList

    xs.take(26) shouldBe Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    xs.slice(26, 36) shouldBe Seq("AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AI", "AJ")
    xs.slice(52, 62) shouldBe Seq("BA", "BB", "BC", "BD", "BE", "BF", "BG", "BH", "BI", "BJ")
  }

  behavior of "transform"

  private val movieHeader = "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes"

  it should "parse and transform the following rows with pushdown function" in {
    import RawParsers.WithHeaderRow._

    val rows = Seq(
      movieHeader,
      ",Doug Walker,,,131,,Rob Walker,131,,Documentary,Doug Walker,Star Wars: Episode VII - The Force Awakens             ,8,143,,0,,https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0"
    )

    val mty: Try[RawTable] = Table.parse(rows)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    val rawTable: RawTable = mty.get
    rawTable.size shouldBe 1
    rawTable.head(1) shouldBe "Doug Walker"

    val f = RawTableTransformation(Map("MOVIE_TITLE" -> CellTransformation(_.toLowerCase)))
    f.apply(rawTable).head(11) shouldBe "star wars: episode vii - the force awakens             "
  }

  behavior of "projection"

  it should "parse and project the following rows" in {
    import RawParsers.WithHeaderRow._

    val rows = Seq(
      movieHeader,
      ",Doug Walker,,,131,,Rob Walker,131,,Documentary,Doug Walker,Star Wars: Episode VII - The Force Awakens             ,8,143,,0,,https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0"
    )

    val mty: Try[RawTable] = Table.parse(rows)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    val rawTable: RawTable = mty.get
    rawTable.size shouldBe 1
    rawTable.head(1) shouldBe "Doug Walker"

    val f = RawTableProjection(Seq("MOVIE_TITLE"))
    f.apply(rawTable).head.head shouldBe "Star Wars: Episode VII - The Force Awakens             "
  }

  behavior of "sort"

  it should "sort a Table" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99", "1 3"))
    implicit object IntPairOrdering extends Ordering[IntPair] {
      def compare(x: IntPair, y: IntPair): Int = x.a.compareTo(y.a) match {
        case 0 => x.b.compareTo(y.b)
        case cf => cf
      }
    }
    println(iIty map (_.sorted))
  }

  behavior of "parseResourceRaw"
  it should "parse quotes spanning newlines" in {
    val mty: Try[RawTable] = Table.parseResourceRaw("multiline.csv", TableParser.includeAll)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    mty match {
      case Success(HeadedTable(r, h)) =>
        println(s"parseResourceRaw: successfully read ${r.size} rows")
        println(s"parseResourceRaw: successfully read ${h.size} columns")
        r.size shouldBe 4
        r take 4 foreach println
    }
  }
}
