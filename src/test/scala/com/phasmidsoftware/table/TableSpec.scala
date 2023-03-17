/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import cats.effect.IO
import cats.implicits.catsSyntaxParallelAp
import com.phasmidsoftware.parse._
import com.phasmidsoftware.render._
import com.phasmidsoftware.table.Table.parseResource
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.{EvaluateIO, TryUsing}
import com.phasmidsoftware.write.{Node, TreeWriter, Writable}
import java.io.{File, FileWriter, InputStream}
import java.net.URL
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec
import org.scalatest.matchers.should
import org.scalatest.time.{Seconds, Span}
import scala.annotation.unused
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class TableSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class IntPair(a: Int, b: Int) {
    def map(f: Int => Int): IntPair = IntPair(f(a), f(b))
  }

  object IntPair {

    object IntPairParser extends JavaTokenParsers {
      lazy val pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    trait IntPairRowParser extends StringParser[IntPair] {
      def parse(indexedString: (String, Int))(header: Header): Try[IntPair] = IntPairParser.parseAll(IntPairParser.pair, indexedString._1) match {
        case IntPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse ${indexedString._1}"))
      }

      //noinspection NotImplementedCode
      def parseHeader(w: Seq[String]): IO[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      type Row = IntPair

      val maybeFixedHeader: Option[Header] = Some(Header.create("a", "b"))

      val headerRowsToRead: Int = 0

      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, Header[IntPair]())

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "Table"

  it should "parse from Seq[String]" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  it should "parse input stream" in {
    import IntPair._
    matchIO(Table.parseInputStream(IO(classOf[TableSpec].getResourceAsStream("intPairs.csv")), "UTF-8")) {
      case HeadedTable(_, _) => succeed
    }
  }

  it should "parse and not filter the movies from the IMDB dataset" in {
    import MovieParser._
    implicit val hasKey: HasKey[Movie] = (t: Movie) => t.production.country
    matchIO(parseResource("movie_metadata.csv", classOf[Movie]), Timeout(Span(3, Seconds))) {
      case mt@HeadedTable(_, _) =>
        val kiwiMovies = mt.filterNotByKey(_ == "New Zealand")
        kiwiMovies.size shouldBe 1563
    }
  }

  it should "parse table using URI and encryption" in {
    import IntPair._
    val url = classOf[TableSpec].getResource("intPairs.csv")
    matchIO(Table.parse(url.toURI, "ISO-8859-1")) {
      case HeadedTable(_, _) => succeed
    }
  }

  it should "parse table from file" in {
    import IntPair._
    val z1 = Table.parseFile("src/test/resources/com/phasmidsoftware/table/intPairs.csv", "UTF-8")
    val z2 = Table.parseFile("src/test/resources/com/phasmidsoftware/table/intPairs.csv")
    matchIO(z1 parProduct z2) {
      case (a@HeadedTable(_, _), b@HeadedTable(_, _)) => a.size shouldBe 2; b.size shouldBe 2
    }
  }

  // NOTE: this test can be flaky. Perhaps we should just use zip instead of parProduct.
  it should "parse table from raw file" in {
    val z1: IO[Table[RawRow]] = Table.parseFileRaw(new File("output.csv"), TableParser.includeAll, Some(Header(Seq(Seq("a", "b")))))
    val z2: IO[Table[RawRow]] = Table.parseFileRaw("src/test/resources/com/phasmidsoftware/table/intPairs.csv", TableParser.includeAll)
    matchIO(z1 parProduct z2) {
      case (a@HeadedTable(_, _), b@HeadedTable(_, _)) =>
        a.size shouldBe 0; b.size shouldBe 1
    }
  }

  it should "write table to the file" in {
    val hdr = Header(Seq(Seq("a", "b")))
    val row1 = Row(Seq("1", "2"), hdr, 1)
    val table = Table(Seq(row1), Some(hdr))
    val resultIO = for {_ <- Table.writeCSVFileRow(table, new File("output.csv"))
                        _ = println(s"written to file output.csv")
                        y <- Table.parseFileRaw("output.csv", TableParser.includeAll)
                        } yield y
    matchIO(resultIO) {
      case xt@HeadedTable(_, _) => xt.content.head.toString() shouldBe """A="1", B="2""""
    }
    val tableWithoutHead = Table(Seq(row1), None)
    the[TableException] thrownBy Table.writeCSVFileRow(tableWithoutHead, new File("output.csv"))
  }

  it should "parse from Iterator[String]" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99").iterator)) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  it should "parse from Source" in {
    import IntPair._

    val source = Source.fromChars(Array('1', ' ', '2', '\n', '4', '2', ' ', '9', '9', '\n'))
    matchIO(Table.parseSource(source)) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  it should "parse from File" in {
    import IntPair._

    matchIO(Table.parseFile(new File("src/test/resources/com/phasmidsoftware/table/intPairs.csv"))) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  it should "parse raw resource using table parser " in {
    matchIO(Table.parseResourceRaw("intPairs.csv", TableParser.includeAll)) {
      case HeadedTable(_, _) => succeed
    }
  }

  it should "parse from null File" in {
    import IntPair._

    val f: String = null
    import cats.effect.unsafe.implicits.global
    EvaluateIO.checkFailure(Table.parseFile(new File(f)))(classOf[NullPointerException]).unsafeRunSync()
  }

  it should "parse from Resource" in {
    import IntPair._

    matchIO(Table.parseResource("intPairs.csv", classOf[TableSpec])) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  it should "zip tables" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99").iterator)) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
        xt.zip(xt).content.toSeq shouldBe Seq((IntPair(1, 2), IntPair(1, 2)), (IntPair(42, 99), IntPair(42, 99)))
    }
  }

  it should "Throw table exception" in {
    TableException.apply("exception thrown").w shouldBe "exception thrown"
  }

  it should "headed table object" in {
    HeadedTable.apply(Seq(0, 1), Header.create("r", "i")).content.toSeq shouldBe Seq(0, 1)
  }

  behavior of "Unheaded Table"

  it should "unheaded table" in {
    val ut = UnheadedTable(Seq(1))
    ut.unit(Seq(2), Some(Header.create("x"))) shouldBe HeadedTable(Seq(2), Header.create("x"))
    ut.unit(Seq(2), None) shouldBe UnheadedTable(Seq(2))
  }

  it should "unheaded column table" in {
    val ut = UnheadedTable(Seq(1))
    ut.column("x") shouldBe Iterator.empty
  }

  it should "mapOptional" in {
    val ut = UnheadedTable(Seq(1, 2, 3, 4, 5))

    def isEven(x: Int): Option[Int] = if (x % 2 == 0) Some(x) else None

    val result: Table[Int] = ut.mapOptional(isEven)
    result.content.toSeq shouldBe List(2, 4)
  }

  behavior of "parse with safeResource"

  it should "return success for intPairs.csv" in {
    import IntPair._

    lazy val i: InputStream = classOf[TableSpec].getResourceAsStream("intPairs.csv")
    matchIO(Table.parseInputStream(i)) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  it should "return failure(0)" in {
    import IntPair._

    val iIty = Table.parse(IO(Source.fromResource(null)))
    import cats.effect.unsafe.implicits.global
    EvaluateIO.checkFailure(iIty)(classOf[NullPointerException]).unsafeRunSync()
  }

  it should "return failure(1)" in {
    import IntPair._

    lazy val si: IO[InputStream] = IO(classOf[TableSpec].getResourceAsStream(null))
    val iIty = for (s <- si) yield Table.parseInputStream(s)
    import cats.effect.unsafe.implicits.global
    EvaluateIO.checkFailure(iIty)(classOf[NullPointerException]).unsafeRunSync()
  }

  it should "return failure(2)" in {
    // NOTE this uses Try (as it always did) and not IO.
    lazy val si: InputStream = getClass.getResourceAsStream("emptyResource.txt")
    val wy: Try[String] = TryUsing(Source.fromInputStream(si))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NoSuchElementException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return success for intPairs.csv URL with encoding" in {
    import IntPair._

    lazy val u: URL = classOf[TableSpec].getResource("intPairs.csv")
    matchIO(Table.parseResource(u, "UTF-8")) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  behavior of "other"

  it should "do iterator" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
        val x = xt.iterator
        x.hasNext shouldBe true
        x.next() shouldBe IntPair(1, 2)
        x.hasNext shouldBe true
        x.next() shouldBe IntPair(42, 99)
        x.hasNext shouldBe false
    }
  }

  it should "map" in {
    val f: IntPair => IntPair = _ map (_ * 2)

    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.map(f).content.toSeq shouldBe Seq(IntPair(2, 4), IntPair(84, 198))
    }
  }

  it should "flatMap" in {
    val f: IntPair => Table[IntPair] = p => HeadedTable(Seq(p), Header())

    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.flatMap(f).content.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "to Seq" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "to Shuffle" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.shuffle.content.size shouldBe 2
    }
  }

  it should "drop" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.drop(1).content.toSeq shouldBe Seq(IntPair(42, 99))
    }
  }

//  it should "dropRight" in {
//    import IntPair._
//    matchIO(Table.parse(Seq("1 2", "42 99"))) {
//      case xt@HeadedTable(_, _) =>
//        xt.dropRight(1).rows shouldBe Seq(IntPair(1, 2))
//    }
//  }

  it should "empty" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.empty.content.toSeq shouldBe Seq.empty
    }
  }

  it should "dropWhile" in {
    import IntPair._
    matchIO(Table.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.dropWhile(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "filter" in {
    import IntPair._
    matchIO(Table.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.filter(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(3, 4))
    }
  }

  it should "filterNot" in {
    import IntPair._
    matchIO(Table.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.filterNot(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "slice" in {
    import IntPair._
    matchIO(Table.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.slice(0, 2).content.toSeq shouldBe Seq(IntPair(3, 4), IntPair(1, 2))
    }
  }
//
//  it should "takeRight" in {
//    import IntPair._
//    matchIO(Table.parse(Seq("3 4", "1 2", "42 99"))) {
//      case xt@HeadedTable(_, _) =>
//        xt.takeRight(2).rows shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
//    }
//  }

  it should "takeWhile" in {
    import IntPair._
    matchIO(Table.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.takeWhile(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(3, 4))
    }
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

  // FIXME this is a mystery
  ignore should "render the table to CSV" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case HeadedTable(_, _) => succeed
      case x => fail(s"error: $x")
    }

    implicit object StringBuilderWritable extends Writable[StringBuilder] {
      def unit: StringBuilder = new StringBuilder

      override def delimiter: CharSequence = "|"

      def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
    }

    implicit object DummyRenderer$$ extends Renderer[Table[IntPair], String] {
      def render(t: Table[IntPair], attrs: Map[String, String]): String =
        t match {
          case t: RenderableTable[IntPair] => t.renderToWritable(StringBuilderWritable).toString
          case _ => throw TableException("render problem")
        }
    }

    val wi: IO[String] = Table.parse(Seq("1 2", "42 99")) map {
      case r: Table[IntPair] => implicitly[Renderer[Table[IntPair], String]].render(r)
      case _ => fail("cannot render table")
    }
    matchIO(wi) {
      case "a|b\n1|2\n42|99\n" => succeed
      case x => fail(s"string is $x")
    }
  }

  it should "render the table to CSV using a Writable" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99"))) {
      case HeadedTable(_, _) => succeed
    }

    val file = new File("output.csv")
    implicit val fw: Writable[FileWriter] = Writable.fileWritable(file)

    implicit object FileRenderer extends Renderer[Table[IntPair], FileWriter] {
      def render(t: Table[IntPair], attrs: Map[String, String]): FileWriter = t match {
        case pr: RenderableTable[IntPair] => pr.renderToWritable
        case _ => throw TableException("render problem")
      }
    }

    //    implicit object FileCsvRenderer extends CsvRenderer[Table[IntPair]] {
    //      def render(t: Table[IntPair], attrs: Map[String, String]): FileWriter = t match {
    //        case pr: RenderableTable[IntPair] => pr.renderToWritable
    //        case _ => throw TableException("render problem")
    //      }
    //    }

    val fi: IO[FileWriter] = Table.parse(Seq("1 2", "42 99")) map {
      case r: Table[IntPair] =>
        val z: Renderer[Table[IntPair], FileWriter] = implicitly[Renderer[Table[IntPair], FileWriter]]
        z.render(r)
      case _ => fail("cannot render table")
    }
    matchIO(fi) {
      case _ => succeed
    }
  }

  it should "render another parsed table to CSV" in {
    import IntPair._

    implicit object IntPairCsvRenderer extends CsvRenderer[IntPair] {
      val csvAttributes: CsvAttributes = CsvAttributes(", ")

      def render(t: IntPair, attrs: Map[String, String]): String = s"${t.a}${csvAttributes.delimiter}${t.b}"
    }

    implicit object IntPairCsvGenerator extends CsvProductGenerator[IntPair] {
      val csvAttributes: CsvAttributes = CsvAttributes(", ")

      def toColumnNames(po: Option[String], no: Option[String]): String = s"a${csvAttributes.delimiter}b"
    }

    implicit val csvAttributes: CsvAttributes = IntPairCsvRenderer.csvAttributes
    matchIO(Table.parseFile(new File("src/test/resources/com/phasmidsoftware/table/intPairs.csv"))) {
      case iIt@HeadedTable(_, _) =>
        val ws = iIt.toCSV
        EvaluateIO(ws) shouldBe "a, b\n1, 2\n42, 99\n"
    }
  }

  it should "render another parsed table to CSV with delim, quote" in {
    import IntPair._
    implicit val myCsvAttributes: CsvAttributes = CsvAttributes("|")

    implicit object IntPairCsvRenderer extends CsvRenderer[IntPair] {
      val csvAttributes: CsvAttributes = myCsvAttributes

      def render(t: IntPair, attrs: Map[String, String]): String = s"${t.a}${csvAttributes.delimiter}${t.b}"
    }

    implicit object IntPairCsvGenerator extends CsvProductGenerator[IntPair] {
      val csvAttributes: CsvAttributes = myCsvAttributes

      def toColumnNames(wo: Option[String], no: Option[String]): String = s"a${csvAttributes.delimiter}b"
    }

    matchIO(Table.parseFile(new File("src/test/resources/com/phasmidsoftware/table/intPairs.csv"))) {
      case iIt@HeadedTable(_, _) =>
        val ws = iIt.toCSV
        EvaluateIO(ws) shouldBe "a|b\n1|2\n42|99\n"
    }
  }

  // FIXME ...
  it should "render the parsed table with TreeWriter" in {
    import IntPair._
    @unused
    val iIty: IO[Table[IntPair]] = Table.parse(Seq("1 2", "42 99"))
//      val hy = iIty map {
//        case r: HeadedTable[IntPair] => r.render
//      }
//      hy should matchPattern { case Success(_) => }
//      // CONSIDER why do we use ArrayBuffer here instead of List?
//      hy.get shouldBe HTML("table", None, Map(), List(HTML("thead", None, Map(), List(HTML("tr", None, Map(), Seq(HTML("th", Some("a"), Map(), List()), HTML("th", Some("b"), Map(), List()))))), HTML("tbody", None, Map(), List(HTML("IntPair", None, Map(), List(HTML("", Some("1"), Map("name" -> "a"), List()), HTML("", Some("2"), Map("name" -> "b"), List()))), HTML("IntPair", None, Map(), List(HTML("", Some("42"), Map("name" -> "a"), List()), HTML("", Some("99"), Map("name" -> "b"), List())))))))
  }

  def mapTo[T, U](ty: Try[T]): Try[U] = ty match {
    case Success(t) => Success(t.asInstanceOf[U])
    case Failure(x) => Failure(x)
  }

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

  it should "create ++" in {
    val header = Header(letters = true, 1)
    header.++(header).xs shouldBe Seq("A", "A")
  }

  behavior of "transform"

  private val movieHeader = "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes"

  it should "parse and transform the following rows with pushdown function" in {
    import RawParsers.WithHeaderRow._

    val rows = Seq(
      movieHeader,
      ",Doug Walker,,,131,,Rob Walker,131,,Documentary,Doug Walker,Star Wars: Episode VII - The Force Awakens             ,8,143,,0,,https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0"
    )

    matchIO(Table.parse(rows)) {
      case mt@HeadedTable(_, _) =>
        mt.size shouldBe 1
        mt.head(1).get shouldBe "Doug Walker"

        val f = RawTableTransformation(Map("MOVIE_TITLE" -> CellTransformation(_.toLowerCase)))
        f.apply(mt).head(11).get shouldBe "star wars: episode vii - the force awakens             "
    }

  }

  behavior of "projection"

  it should "parse and project the following rows" in {
    import RawParsers.WithHeaderRow._

    val rows = Seq(
      movieHeader,
      ",Doug Walker,,,131,,Rob Walker,131,,Documentary,Doug Walker,Star Wars: Episode VII - The Force Awakens             ,8,143,,0,,https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0"
    )

    matchIO(Table.parse(rows)) {
      case rawTable@HeadedTable(_, _) =>
        rawTable.size shouldBe 1
        rawTable.head(1).get shouldBe "Doug Walker"

        val f = RawTableProjection(Seq("MOVIE_TITLE"))
        f.apply(rawTable).head.ws.head shouldBe "Star Wars: Episode VII - The Force Awakens             "
    }
  }

  behavior of "sort"

  it should "sort a Table and then select" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99", "1 3"))) {
      case mt: Table[IntPair] =>

        implicit object IntPairOrdering extends Ordering[IntPair] {
          def compare(x: IntPair, y: IntPair): Int = x.a.compareTo(y.a) match {
            case 0 => x.b.compareTo(y.b)
            case cf => cf
          }
        }
        val x: Table[IntPair] = mt.sort
        val row1 = x.select(Range(2, 3))
        row1.size shouldBe 1
        row1.head shouldBe IntPair(1, 3)
    }
  }

  behavior of "select"

  it should "select from a Table" in {
    import IntPair._
    matchIO(Table.parse(Seq("1 2", "42 99", "1 3"))) {
      case table: Table[IntPair] =>
        table.select(1).size shouldBe 1
        table.select(1).head shouldBe IntPair(1, 2)
        table.select(2).head shouldBe IntPair(42, 99)
        table.select(3).head shouldBe IntPair(1, 3)
        val row1 = table.select(Range(3, 4))
        row1.size shouldBe 1
        row1.head shouldBe IntPair(1, 3)
        val row2 = table.select(Range(2, 3))
        row2.size shouldBe 1
        row2.head shouldBe IntPair(42, 99)
        val rows02 = table.select(Range(1, 4, 2))
        rows02.size shouldBe 2
    }
  }

  behavior of "parseResourceRaw"
  it should "parse quotes spanning newlines" in {
    val parser = RawTableParser(TableParser.includeAll, None).setMultiline(true)
    import com.phasmidsoftware.util.FP.resource

    val sy: IO[Source] = IO.fromTry(resource[TableSpec]("multiline.csv") map Source.fromURL)
    matchIO(parser.parse(sy)) {
      case HeadedTable(r, h) =>
        println(s"parseResourceRaw: successfully read ${r.size} rows")
        println(s"parseResourceRaw: successfully read ${h.size} columns")
        r.size shouldBe 4
        r take 4 foreach println
        succeed
      case _ => fail("should succeed")
    }
  }

  behavior of "Header"
  it should "do lookup" in {
    val hdr = Header(Seq(Seq("a", "Hello Goodbye", "Team Number")))
    hdr.getIndex("a") shouldBe Success(0)
    hdr.getIndex("Hello Goodbye") shouldBe Success(1)
    hdr.getIndex("team number") shouldBe Success(2)
  }

  behavior of "Table[Row]"
  it should "work" in {
    val hdr = Header(Seq(Seq("a", "b")))
    val row1 = Row(Seq("1", "2"), hdr, 1)
    val table = Table(Seq(row1), Some(hdr))
    EvaluateIO(Table.toCSVRow(table)) shouldBe "a,b\n1,2\n"
  }
}
