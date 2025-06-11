/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.examples.Movie.MovieRowProcessor
import com.phasmidsoftware.tableparser.core.table.Table.{parseSequence, sourceFromClassResource}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import com.phasmidsoftware.tableparser.core.util.TryUsing
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.io.{Codec, Source}
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

//noinspection SpellCheckingInspection
class TableParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class IntPair(a: Int, b: Int)

  //noinspection ScalaUnusedSymbol
  // NOTE: unused
  private val dateFormatString = "MM/dd/yyyy"

  object IntPair {

    object IntPairParser extends JavaTokenParsers {
      def pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    trait IntPairRowParser extends StringParser[IntPair] {
      def parse(header: Header)(input: String): Try[IntPair] = IntPairParser.parseAll(IntPairParser.pair, input) match {
        case IntPairParser.Success((x: Int, y: Int), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse ${input}"))
      }

      def parseIndexed(header: Header)(indexedString: (String, Int)): Try[IntPair] = parse(header)(indexedString._1)

      //noinspection NotImplementedCode
      def parseHeader(w: Seq[String]): Try[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, header)

      type Row = IntPair

      val maybeHeader: Option[Header] = Some(Header.create("x", "y"))

      override val headerRowsToRead: Int = 0

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "IntPair TableParser"

  it should "parse int pair" in {

    import IntPair._

    val strings: Seq[String] = Seq("1 2")
    matchTry(Table.parse(strings)) {
      case t: Table[IntPair] =>
        t.toSeq shouldBe List(IntPair(1, 2))
    }
  }

  behavior of "TableParser with StandardRowParser"

  /**
   * CONSIDER using the DailyRaptorReport defined in its own module.
   */
  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object DailyRaptorReport {

    object DailyRaptorReportParser extends CellParsers {


      private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

      def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

      implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
      implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
      implicit val dailyRaptorReportParser: CellParser[DailyRaptorReport] = cellParser4(DailyRaptorReport.apply)
    }

    import DailyRaptorReportParser._

    trait DailyRaptorReportConfig extends DefaultRowConfig {
      override val string: Regex = """[\w/\- ]+""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit object DailyRaptorReportConfig extends DailyRaptorReportConfig

    implicit val parser: StandardRowParser[DailyRaptorReport] = StandardRowParser[DailyRaptorReport](LineParser.apply)

    trait DailyRaptorReportTableParser extends StringTableParser[Table[DailyRaptorReport]] {
      type Row = DailyRaptorReport

      val maybeHeader: Option[Header] = None

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

      protected def builder(rows: Iterable[DailyRaptorReport], header: Header): Table[Row] = HeadedTable(rows, header)
    }

    implicit object DailyRaptorReportTableParser extends DailyRaptorReportTableParser

  }

  behavior of "RowParser.parse"

  private val headerRaptors = "Date\tWeather\tWnd Dir\tWnd Spd\tBV\tTV\tUV\tOS\tBE\tNH\tSS\tCH\tGO\tUA\tRS\tBW\tRT\tRL\tUB\tGE\tUE\tAK\tM\tP\tUF\tUR\tOth\tTot"
  private val partlyCloudy = "Partly Cloudy"

  it should "parse regex string" in {
    import DailyRaptorReport._

    val rowParser = implicitly[RowParser[DailyRaptorReport, String]]
    val firstRow = headerRaptors
    val row = "09/16/2018\t" + partlyCloudy + "\tSE\t6-12\t0\t0\t0\t4\t19\t3\t30\t2\t0\t0\t2\t3308\t5\t0\t0\t0\t0\t27\t8\t1\t0\t1\t0\t3410"
    matchTry(rowParser.parseHeader(Seq(firstRow))) {
      case header@Header(_, _) =>
        val hawkCount: Try[DailyRaptorReport] = parser.parseIndexed(header)((row, 0))
        hawkCount should matchPattern { case Success(DailyRaptorReport(_, `partlyCloudy`, 3308, 5)) => }
    }
  }

  behavior of "Table.parse"

  it should "parse raptors from raptors.csv" in {
    import DailyRaptorReport._

    matchTry(for (r <- Table.parseResource(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.content.size shouldBe 13
        // TODO fix deprecation. Also in two other places in this module.
        //noinspection ScalaDeprecation
        val date = new LocalDate(2018, 9, 12)
        rt.content.head shouldBe DailyRaptorReport(date, "Dense Fog/Light Rain", 0, 0)
    }
  }

  it should "parse raptors from Seq[String]" in {
    import DailyRaptorReport._

    val raw = Seq(headerRaptors,
      "09/16/2018\t" + partlyCloudy + "\tSE\t6-12\t0\t0\t0\t4\t19\t3\t30\t2\t0\t0\t2\t3308\t5\t0\t0\t0\t0\t27\t8\t1\t0\t1\t0\t3410",
      "09/19/2018\tOvercast/Mostly cloudy/Partly cloudy/Clear\tNW\t4-7\t0\t0\t0\t47\t12\t0\t84\t10\t0\t0\t1\t821\t4\t0\t1\t0\t0\t27\t4\t1\t0\t2\t0\t1014")
    matchTry(for (r <- Table.parse(raw)) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.content.size shouldBe 2
        //noinspection ScalaDeprecation
        val date = new LocalDate(2018, 9, 16)
        rt.content.head shouldBe DailyRaptorReport(date, partlyCloudy, 3308, 5)
    }
  }

  it should "fail empty sequence" in {
    import DailyRaptorReport._

    Table.parse(Seq(headerRaptors, "")) should matchPattern { case Failure(e: ParserException) => }
  }

  it should "parse empty sequence" in {

    val raw = Seq(headerRaptors,
      "")
    matchTry(for (r <- Table.parseRaw(raw, TableParser.includeAll)) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.size shouldBe 0
    }
  }

  object DailyRaptorReportSeq {
    val header: Seq[String] = Seq("date", "weather", "bw", "ri")

    object DailyRaptorReportParser extends CellParsers {

      private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

      def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

      implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
      implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
      implicit val dailyRaptorReportParser: CellParser[DailyRaptorReport] = cellParser4(DailyRaptorReport.apply)
    }

    import DailyRaptorReportParser._

    trait DailyRaptorReportConfig extends DefaultRowConfig {
      override val string: Regex = """[\w/\- ]+""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit object DailyRaptorReportConfig extends DailyRaptorReportConfig

    implicit val parser: StandardStringsParser[DailyRaptorReport] = StandardStringsParser[DailyRaptorReport]()

    trait DailyRaptorReportStringsTableParser extends StringsTableParser[Table[DailyRaptorReport]] {
      type Row = DailyRaptorReport

      val maybeHeader: Option[Header] = None

      val rowParser: RowParser[Row, Seq[String]] = implicitly[RowParser[Row, Seq[String]]]

      protected def builder(rows: Iterable[DailyRaptorReport], header: Header): Table[Row] = HeadedTable(rows, header)
    }

    implicit object DailyRaptorReportStringsTableParser extends DailyRaptorReportStringsTableParser

  }

  it should "parse raptors from Seq[Seq[String]]" in {
    import DailyRaptorReportSeq._

    val raw = Seq(Seq("Date", "Weather", "Wnd Dir", "Wnd Spd", "BV", "TV", "UV", "OS", "BE", "NH", "SS", "CH", "GO", "UA", "RS", "BW", "RT", "RL", "UB", "GE", "UE", "AK", "M", "P", "UF", "UR", "Oth", "Tot"),
      Seq("09/16/2018", partlyCloudy, "SE", "6-12", "0", "0", "0", "4", "19", "3", "30", "2", "0", "0", "2", "3308", "5", "0", "0", "0", "0", "27", "8", "1", "0", "1", "0", "3410"),
      Seq("09/19/2018", "Overcast/Mostly cloudy/Partly cloudy/Clear", "NW", "4-7", "0", "0", "0", "47", "12", "0", "84", "10", "0", "0", "1", "821", "4", "0", "1", "0", "0", "27", "4", "1", "0", "2", "0", "1014"))
    matchTry(for (r <- Table.parseSequence(raw.iterator)) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.content.size shouldBe 2
        //noinspection ScalaDeprecation
        val date = new LocalDate(2018, 9, 16)
        rt.content.head shouldBe DailyRaptorReport(date, partlyCloudy, 3308, 5)
    }
  }

  object DailyRaptorReportNoHeader {
    val header: Seq[String] = Seq("Date", "Weather", "Wnd Dir", "Wnd Spd", "BV", "TV", "UV", "OS", "BE", "NH", "SS", "CH", "GO", "UA", "RS", "BW", "RT", "RL", "UB", "GE", "UE", "AK", "M", "P", "UF", "UR", "Oth", "Tot")

    object DailyRaptorReportParser extends CellParsers {

      private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

      def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

      implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
      implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
      implicit val dailyRaptorReportParser: CellParser[DailyRaptorReport] = cellParser4(DailyRaptorReport.apply)
    }

    import DailyRaptorReportParser._

    trait DailyRaptorReportConfig extends DefaultRowConfig {
      override val string: Regex = """[\w/\- ]+""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit object DailyRaptorReportConfig extends DailyRaptorReportConfig

    implicit val parser: StandardRowParser[DailyRaptorReport] = StandardRowParser[DailyRaptorReport](LineParser.apply)

    trait DailyRaptorReportTableParser extends StringTableParser[Table[DailyRaptorReport]] {
      type Row = DailyRaptorReport

      val maybeHeader: Option[Header] = Some(Header.create(header: _*))

      override val headerRowsToRead: Int = 0

      protected def builder(rows: Iterable[DailyRaptorReport], header: Header): Table[DailyRaptorReport] = HeadedTable(rows, header)

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object DailyRaptorReportTableParser extends DailyRaptorReportTableParser

  }


  it should "parse raptors without header" in {
    import DailyRaptorReportNoHeader._

    matchTry(for (r <- Table.parseResource("noHeader.csv", classOf[TableParserSpec])) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.content.size shouldBe 13
        // TODO fix deprecation. Also in two other places in this module.
        val date = new LocalDate(2018, 9, 12)
        rt.content.head shouldBe DailyRaptorReport(date, "Dense Fog/Light Rain", 0, 0)
    }
  }

  case class Submission(username: String, lastName: String, firstName: String, questions: Seq[Question])

  object Submission extends CellParsers {
    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(ColumnHelper.camelCaseColumnNameMapperSpace, Some("$c $x"))
    implicit val submissionParser: CellParser[Submission] = cellParser4(apply)
    implicit val parser: StandardStringsParser[Submission] = StandardStringsParser[Submission]()

    implicit object TableParser extends StringsTableParser[Table[Submission]] {
      type Row = Submission

      val maybeHeader: Option[Header] = None // Some(header)

      protected def builder(rows: Iterable[Row], header: Header): Table[Row] = HeadedTable(rows, header)

      override val forgiving: Boolean = false

      val rowParser: RowParser[Row, Seq[String]] = implicitly[RowParser[Row, Seq[String]]]
    }
  }

  case class Question(questionId: String, question: String, answer: Option[String], possiblePoints: Int, autoScore: Option[Double], manualScore: Option[Double])

  object Question extends CellParsers {
    private val mapper: String => String = _.replaceAll("(_)", " ")
    implicit val helper: ColumnHelper[Question] = columnHelper(mapper, Some("$c $x"), "questionId" -> "question_ID")
    implicit val optParserString: CellParser[Option[String]] = cellParserOption
    implicit val parser: CellParser[Question] = cellParser6(apply)
    implicit val seqParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
  }

  behavior of "TableParser"

  it should "parse Submission" in {

    val rows: Seq[Seq[String]] = Seq(
      Seq("Username", "Last Name", "First Name", "Question ID 1", "Question 1", "Answer 1", "Possible Points 1", "Auto Score 1", "Manual Score 1"),
      Seq("001234567s", "Mr.", "Nobody", "Question ID 1", "The following are all good reasons to learn Scala -- except for one.", "Scala is the only functional language available on the Java Virtual Machine", "4", "4", "")
    )

    // TODO note that the column lookup isn't correct for Question ID 1
    import Submission.TableParser
    matchTry(Table.parseSequence(rows.iterator)) {
      case rt@HeadedTable(_, _) =>
        println(rt.head)
        rt.size shouldBe 1
    }
  }

  it should "fail on incompatible parser" in {
    import Submission.TableParser
    val strings: Seq[String] = Nil
    Table.parse(strings) should matchPattern { case Failure(e: ParserException) => }
  }

  it should "fail on empty rows" in {
    import Submission.TableParser
    val rows: Seq[Seq[String]] = Nil
    parseSequence(rows.iterator) should matchPattern { case Failure(e: NoSuchElementException) => }
  }

  behavior of "submissions from file"

  object SubmissionAlt extends CellParsers {

    private val baseColumnNameMapper: String => String = _.replaceAll("(_)", " ")

    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(ColumnHelper.camelCaseColumnNameMapperSpace)
    implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
    implicit val submissionParser: CellParser[Submission] = cellParser4(Submission.apply)

    implicit object SubmissionConfig extends DefaultRowConfig {
      override val string: Regex = """[^\t]*""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit val parser: StandardRowParser[Submission] = StandardRowParser.create[Submission]

    implicit object SubmissionTableParser extends StringTableParser[Table[Submission]] {
      type Row = Submission

      protected def builder(rows: Iterable[Row], header: Header): Table[Submission] = HeadedTable(rows, header)

      val maybeHeader: Option[Header] = None // Some(header)

      override val forgiving: Boolean = true

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

  }

  it should "parse sample.csv with Submission1" in {
    import SubmissionAlt._
    implicit val codec: Codec = Codec("UTF-16") // Not sure why it is in UTF-16 but it is.
    val sy: Try[Source] = Try(Source.fromURL(classOf[TableParserSpec].getResource("submissions.csv")))
    val sti: Try[Table[Submission]] = TryUsing(sy)(s => Table.parseSource(s))
    matchTry(sti) {
      case st@HeadedTable(_, _) => st.size shouldBe 1
    }
  }

  it should "parse sample.csv with Submission1 Alt" in {
    import SubmissionAlt._
    implicit val codec: Codec = Codec("UTF-16") // Not sure why it is in UTF-16 but it is.
    matchTry(Table.parseResource("submissions.csv", classOf[TableParserSpec])) {
      case rt@HeadedTable(_, _) => rt.size shouldBe 1
    }
  }

  /**
   * The following tests relate to the application CsvToJSON
   */
  behavior of "TableParserHelper"

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends TableParserHelper[Player]() {
    def cellParser: CellParser[Player] = cellParser2(apply)

    /**
     * Method to transform a Table[Player] into a Table[Partnership].
     *
     * The requirements of the application are that the rows of the Player table are grouped by twos
     * and each resulting entity (an array of length 2) is taken to form a Partnership.
     *
     * CONSIDER merging with duplicate code in: TableParserHelperSpec.scala
     *
     * @param pt a Table[Player]
     * @return a Table[Partnership]
     */
    def convertTable(pt: Table[Player]): Table[Partnership] = pt.processRows(xs => Content((xs.toSeq grouped 2).map(r => Partnership(r)).toList))
//      pt.processRows(xs => (xs.toSeq grouped 2)).map(r => Partnership(r))
  }

  case class Partnership(playerA: String, playerB: String) {
    val asArray: Array[String] = Array(playerA, playerB)
  }

  object Partnership {
    def apply(players: Iterable[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
  }

  case class Partnerships(partners: Array[Array[String]]) {
    def size: Int = partners.length
  }

  it should "support header defined in a header row in the input" in {
    val strings = List("First, Last", "Adam,Sullivan", "Amy,Avagadro", "Ann,Peterson", "Barbara,Goldman")
    matchTry(Table.parse[Table[Player]](strings.iterator)) {
      case pt@HeadedTable(_, _) =>
        val partnerships: Partnerships = Partnerships((for (t <- Player.convertTable(pt)) yield t.asArray).toArray)
        partnerships.size shouldBe 2
        partnerships.partners.head shouldBe Array("Adam S", "Amy A")
        partnerships.partners.last shouldBe Array("Ann P", "Barbara G")
    }
  }

  behavior of "RawTableParser"
  it should "header should be set" in {
    val parser = RawTableParser(TableParser.includeAll, None).setMultiline(true)
    val hdr = Header(Seq(Seq("a")))
    parser.setHeader(hdr) shouldBe RawTableParser(maybeHeader = Some(hdr)).setMultiline(true)
  }

  it should "it should set forgiving" in {
    val parser = RawTableParser(TableParser.includeAll, None).setMultiline(true)
    parser.setForgiving(true) shouldBe RawTableParser(TableParser.includeAll, None, forgiving = true, multiline = true)
  }

  it should "it should set row parser of RawTableParser" in {
    val parser = RawTableParser(TableParser.includeAll, None).setPredicate(TableParser.sampler(2))
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.rawRowCellParser
    val lineParser: LineParser = LineParser.apply(rowConfig)
    parser.setRowParser(StandardRowParser[RawRow](lineParser)) shouldBe parser
  }

  behavior of "PlainTextHeadedStringTableParser"
  it should "it should set header of plaintext" in {
    val hdr = Header(Seq(Seq("a")))
    val parser = PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
    parser.setHeader(hdr) shouldBe PlainTextHeadedStringTableParser[Int](Some(hdr), forgiving = false, 1)
  }

  it should "it should set forgiving of plaintext" in {
    val parser = PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1).setMultiline(true)
    parser.setForgiving(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = true, 1)
  }

  it should "it should set predicate of plaintext" in {
    val parser = PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1).setMultiline(true)
    parser.setPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set row parser of PlainTextHeadedStringTableParser" in {
    val parser = PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1).setMultiline(true)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    val lineParser: LineParser = LineParser.apply(rowConfig)
    parser.setRowParser(StandardRowParser[Int](lineParser)) shouldBe parser
  }

  behavior of "MovieRowProcessor"

  it should "process the movies from the IMDB dataset" in {
    val movieRowProcessor: MovieRowProcessor = implicitly[RowProcessor[Movie]].asInstanceOf[MovieRowProcessor]
    val z: Try[Source] = sourceFromClassResource("movie_metadata.csv", classOf[Movie])
    matchTry(z) {
      case source: Source =>
        val iterator: Iterator[String] = source.getLines()
        val ms: Iterator[Movie] = movieRowProcessor.process(iterator, movieRowProcessor.headerRowsToRead)
        (ms to List).size shouldBe 1567
      case _ =>
        fail("unable to open \"movie_metadata.csv\"")
    }
  }

}
