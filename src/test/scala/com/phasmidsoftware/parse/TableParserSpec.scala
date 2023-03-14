/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import cats.effect.IO
import com.phasmidsoftware.crypto.HexEncryption
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.EvaluateIO.{checkFailure, matchIO}
import com.phasmidsoftware.util.IOUsing
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.io.{Codec, Source}
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}
import tsec.cipher.symmetric.jca.AES128CTR

//noinspection SpellCheckingInspection
class TableParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "TableParser"

  case class IntPair(a: Int, b: Int)

  //noinspection ScalaUnusedSymbol
  // NOTE: unused
  private val dateFormatString = "MM/dd/yyyy"

  object IntPair {

    object IntPairParser extends JavaTokenParsers {
      def pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    trait IntPairRowParser extends StringParser[IntPair] {
      def parse(indexedString: (String, Int))(header: Header): Try[IntPair] = IntPairParser.parseAll(IntPairParser.pair, indexedString._1) match {
        case IntPairParser.Success((x: Int, y: Int), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse ${indexedString._1}"))
      }

      //noinspection NotImplementedCode
      override def parseHeader(w: Seq[String]): IO[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, header)

      type Row = IntPair

      val maybeFixedHeader: Option[Header] = Some(Header.create("x", "y"))

      val headerRowsToRead: Int = 0

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  it should "parse int pair" in {

    import IntPair._

    val strings: Seq[String] = Seq("1 2")
    matchIO(Table.parse(strings)) {
      case t: Table[IntPair] =>
        t.rows shouldBe List(IntPair(1, 2))
    }
  }

  behavior of "TableParser with StandardRowParser"

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

      val maybeFixedHeader: Option[Header] = None

      val headerRowsToRead: Int = 1

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
    matchIO(rowParser.parseHeader(Seq(firstRow))) {
      case header@Header(_, _) =>
        val hawkCount: Try[DailyRaptorReport] = parser.parse((row, 0))(header)
        hawkCount should matchPattern { case Success(DailyRaptorReport(_, `partlyCloudy`, 3308, 5)) => }
    }
  }

  behavior of "Table.parse"

  it should "parse raptors from raptors.csv" in {
    import DailyRaptorReport._

    matchIO(for (r <- Table.parseResource(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.rows.size shouldBe 13
        // TODO fix deprecation. Also in two other places in this module.
        //noinspection ScalaDeprecation
        val date = new LocalDate(2018, 9, 12)
        rt.rows.head shouldBe DailyRaptorReport(date, "Dense Fog/Light Rain", 0, 0)
    }
  }

  it should "parse raptors from Seq[String]" in {
    import DailyRaptorReport._

    val raw = Seq(headerRaptors,
      "09/16/2018\t" + partlyCloudy + "\tSE\t6-12\t0\t0\t0\t4\t19\t3\t30\t2\t0\t0\t2\t3308\t5\t0\t0\t0\t0\t27\t8\t1\t0\t1\t0\t3410",
      "09/19/2018\tOvercast/Mostly cloudy/Partly cloudy/Clear\tNW\t4-7\t0\t0\t0\t47\t12\t0\t84\t10\t0\t0\t1\t821\t4\t0\t1\t0\t0\t27\t4\t1\t0\t2\t0\t1014")
    matchIO(for (r <- Table.parse(raw)) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.rows.size shouldBe 2
        //noinspection ScalaDeprecation
        val date = new LocalDate(2018, 9, 16)
        rt.rows.head shouldBe DailyRaptorReport(date, partlyCloudy, 3308, 5)
    }
  }

  it should "fail empty sequence" in {
    import DailyRaptorReport._

    val raw = Seq(headerRaptors,
      "")
    import cats.effect.unsafe.implicits.global
    checkFailure(for (r <- Table.parse(raw)) yield r)(classOf[ParserException]).unsafeRunSync()
  }

  it should "parse empty sequence" in {

    val raw = Seq(headerRaptors,
      "")
    matchIO(for (r <- Table.parseRaw(raw, TableParser.includeAll)) yield r) {
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

      val maybeFixedHeader: Option[Header] = None

      val headerRowsToRead: Int = 1

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
    matchIO(for (r <- Table.parseSequence(raw.iterator)) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.rows.size shouldBe 2
        //noinspection ScalaDeprecation
        val date = new LocalDate(2018, 9, 16)
        rt.rows.head shouldBe DailyRaptorReport(date, partlyCloudy, 3308, 5)
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

      val maybeFixedHeader: Option[Header] = Some(Header.create(header: _*))

      val headerRowsToRead: Int = 0

      protected def builder(rows: Iterable[DailyRaptorReport], header: Header): Table[DailyRaptorReport] = HeadedTable(rows, header)

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object DailyRaptorReportTableParser extends DailyRaptorReportTableParser

  }


  it should "parse raptors without header" in {
    import DailyRaptorReportNoHeader._

    matchIO(for (r <- Table.parseResource("noHeader.csv", classOf[TableParserSpec])) yield r) {
      case rt@HeadedTable(_, _) =>
        rt.rows.size shouldBe 13
        // TODO fix deprecation. Also in two other places in this module.
        val date = new LocalDate(2018, 9, 12)
        rt.rows.head shouldBe DailyRaptorReport(date, "Dense Fog/Light Rain", 0, 0)
    }
  }


  behavior of "StringsParser"

  behavior of "Table"

  case class Question(questionId: String, question: String, answer: Option[String], possiblePoints: Int, autoScore: Option[Double], manualScore: Option[Double])

  case class Submission(username: String, lastName: String, firstName: String, questions: Seq[Question])

  object Submissions extends CellParsers {

    def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")

    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(ColumnHelper.camelCaseColumnNameMapperSpace _, Some("$c $x"))
    implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c $x"), "questionId" -> "question_ID")
    implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
    implicit val questionParser: CellParser[Question] = cellParser6(Question)
    implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
    implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)
    implicit val parser: StandardStringsParser[Submission] = StandardStringsParser[Submission]()

    implicit object SubmissionTableParser extends StringsTableParser[Table[Submission]] {
      type Row = Submission

      val maybeFixedHeader: Option[Header] = None // Some(header)

      val headerRowsToRead: Int = 1

      protected def builder(rows: Iterable[Row], header: Header): Table[Row] = HeadedTable(rows, header)

      override val forgiving: Boolean = false

      val rowParser: RowParser[Row, Seq[String]] = implicitly[RowParser[Row, Seq[String]]]
    }

  }

  it should "parse Submission" in {

    val rows: Seq[Seq[String]] = Seq(
      Seq("Username", "Last Name", "First Name", "Question ID 1", "Question 1", "Answer 1", "Possible Points 1", "Auto Score 1", "Manual Score 1"),
      Seq("001234567s", "Mr.", "Nobody", "Question ID 1", "The following are all good reasons to learn Scala -- except for one.", "Scala is the only functional language available on the Java Virtual Machine", "4", "4", "")
    )

    import Submissions._
    // TODO note that the column lookup isn't correct for Question ID 1

    matchIO(Table.parseSequence(rows.iterator)) {
      case rt@HeadedTable(_, _) =>
        println(rt.head)
        rt.size shouldBe 1
    }
  }

  it should "fail on incompatible parser" in {
    import Submissions._
    val strings: Seq[String] = Nil
    import cats.effect.unsafe.implicits.global
    checkFailure(Table.parse(strings))(classOf[ParserException]).unsafeRunSync()
  }

  it should "fail on empty rows" in {
    import Submissions._
    val rows: Seq[Seq[String]] = Nil
    import cats.effect.unsafe.implicits.global
    checkFailure(Table.parseSequence(rows.iterator))(classOf[NoSuchElementException]).unsafeRunSync()
  }


  behavior of "submissions from file"

  object Submissions1 extends CellParsers {

    def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")

    implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(ColumnHelper.camelCaseColumnNameMapperSpace _)
    implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _)
    implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
    implicit val questionParser: CellParser[Question] = cellParser6(Question)
    implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
    implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)

    implicit object SubmissionConfig extends DefaultRowConfig {
      override val string: Regex = """[^\t]*""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit val parser: StandardRowParser[Submission] = StandardRowParser.create[Submission]

    implicit object SubmissionTableParser extends StringTableParser[Table[Submission]] {
      type Row = Submission

      protected def builder(rows: Iterable[Row], header: Header): Table[Submission] = HeadedTable(rows, header)

      val maybeFixedHeader: Option[Header] = None // Some(header)

      val headerRowsToRead: Int = 1

      override val forgiving: Boolean = true

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

  }

  it should "parse sample.csv with Submission1" in {
    import Submissions1._
    implicit val codec: Codec = Codec("UTF-16") // Not sure why it is in UTF-16 but it is.
    val sy: Try[Source] = Try(Source.fromURL(classOf[TableParserSpec].getResource("submissions.csv")))
    val sti: IO[Table[Submission]] = IOUsing(sy)(s => Table.parseSource(s))
    matchIO(sti) {
      case st@HeadedTable(_, _) => st.size shouldBe 1
    }
  }

  it should "parse sample.csv with Submission1 Alt" in {
    import Submissions1._
    implicit val codec: Codec = Codec("UTF-16") // Not sure why it is in UTF-16 but it is.
    matchIO(Table.parseResource("submissions.csv", classOf[TableParserSpec])) {
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
     * @param pt a Table[Player]
     * @return a Table[Partnership]
     */
    def convertTable(pt: Table[Player]): Table[Partnership] = pt.processRows(xs => (xs grouped 2).toList).map(r => Partnership(r))
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
    matchIO(Table.parse[Table[Player]](strings.iterator)) {
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
    parser.setHeader(hdr) shouldBe RawTableParser(maybeFixedHeader = Some(hdr)).setMultiline(true)
  }

  it should "it should set forgiving" in {
    val parser = RawTableParser(TableParser.includeAll, None).setMultiline(true)
    parser.setForgiving(true) shouldBe RawTableParser(TableParser.includeAll, None, forgiving = true, multiline = true)
  }

  it should "it should set row parser of RawTableParser" in {
    val parser = RawTableParser(TableParser.includeAll, None).setPredicate(TableParser.sampler(2))
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    import RawParsers.WithHeaderRow.rawRowCellParser
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

  implicit val z: HexEncryption[AES128CTR] = com.phasmidsoftware.crypto.EncryptionUTF8AES128CTR

  behavior of "EncryptedHeadedStringTableParser"
  it should "it should set header of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    the[TableParserException] thrownBy parser.setHeader(Header(Seq(Seq("a"))))
  }

  it should "it should set predicate of plaintext" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    parser.setPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set forgiving of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    parser.setForgiving(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = true, 2)
  }

  it should "it should set multiline of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    parser.setMultiline(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set plaintext predicate of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    parser.setPlaintextPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set row parser of EncryptedHeadedStringTableParser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    val lineParser: LineParser = LineParser.apply(rowConfig)
    parser.setRowParser(StandardRowParser[Int](lineParser)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

}
