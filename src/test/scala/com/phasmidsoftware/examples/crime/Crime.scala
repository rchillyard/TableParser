package com.phasmidsoftware.examples.crime

import com.phasmidsoftware.parse._
import com.phasmidsoftware.render._
import com.phasmidsoftware.table.{CsvAttributes, HeadedTable, Header, Table}
import scala.util.Try

/**
 * This example of table parsing is based on the Kaggle data set:
 * [[https://www.kaggle.com/datasets/marshuu/crimes-in-uk-2023?select=2023-01-metropolitan-street.csv]]
 *
 * The file under resources is an edited version of the Metropolitan Crime Statistics 2023-01 (only the first 5,000 rows)
 *
 * @param crimeID             (optional BigInt in hexadecimal notation) see Kaggle.
 * @param month               see Kaggle.
 * @param reportedBy          see Kaggle.
 * @param fallsWithin         see Kaggle.
 * @param longitude           (optional Double) the longitude of the incident.
 * @param latitude            (optional Double) the latitude of the incident.
 * @param location            see Kaggle.
 * @param lsoaCode            see Kaggle.
 * @param lsoaName            see Kaggle.
 * @param crimeType           see Kaggle.
 * @param lastOutcomeCategory see Kaggle.
 * @param context             see Kaggle.
 */
case class Crime(crimeID: Option[BigInt],
                 month: String,
                 reportedBy: String,
                 fallsWithin: String,
                 longitude: Option[Double],
                 latitude: Option[Double],
                 location: String,
                 lsoaCode: String,
                 lsoaName: String,
                 crimeType: String,
                 lastOutcomeCategory: String,
                 context: String) {
  def brief: Option[CrimeLocation] = for (long <- longitude; lat <- latitude) yield CrimeLocation(crimeID, long, lat)
}

case class CrimeLocation(crimeID: Option[BigInt],
                         longitude: Double,
                         latitude: Double) {
}

object CrimeParser extends CellParsers {

  /**
   * Precede each upper case letter (or digit) with _.
   */
  def camelToSnakeCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z\\d])", " $1")

  implicit object BigIntCellParser extends SingleCellParser[BigInt] {
    def convertString(w: String): Try[BigInt] = implicitly[Parseable[BigInt]].parse(w, Some("16"))
  }

  implicit val movieColumnHelper: ColumnHelper[Crime] = columnHelper(camelToSnakeCaseColumnNameMapper _,
    "crimeID" -> "Crime ID")

  implicit val crimeIdParser: CellParser[Option[BigInt]] = cellParserOption[BigInt]
  implicit val movieParser: CellParser[Crime] = cellParser12(Crime.apply)

  implicit object CrimeConfig extends DefaultRowConfig {
    override val listEnclosure: String = ""
  }

  implicit val parser: StandardRowParser[Crime] = StandardRowParser.create[Crime]

  trait CrimeTableParser extends StringTableParser[Table[Crime]] {
    type Row = Crime

    val maybeFixedHeader: Option[Header] = None

    val headerRowsToRead: Int = 1

    override val forgiving: Boolean = true

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    protected def builder(rows: Iterable[Crime], header: Header): Table[Row] = HeadedTable(rows, header)
  }

  implicit object CrimeTableParser extends CrimeTableParser
}

object CrimeRenderer extends CsvRenderers {

  import CsvRenderers._
  import com.phasmidsoftware.render.CsvGenerators._

  private val generators = new CsvGenerators {}

  implicit val bigIntRenderer: CsvRenderer[BigInt] = new CsvRenderer[BigInt] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(t: BigInt, attrs: Map[String, String]): String = t.toString(16)
  }
  implicit val crimeIdRenderer: CsvRenderer[Option[BigInt]] = optionRenderer[BigInt]()
  implicit val crimeIdGenerator: CsvGenerator[Option[BigInt]] = generators.optionGenerator[BigInt]
  implicit val geoRenderer: CsvRenderer[Option[Double]] = optionRenderer[Double]()
  implicit val geoGenerator: CsvGenerator[Option[Double]] = generators.optionGenerator[Double]
  implicit val crimeRenderer: CsvProduct[Crime] = rendererGenerator12(Crime.apply)
}

object CrimeLocationRenderer extends CsvRenderers {

  import CsvRenderers._
  import com.phasmidsoftware.render.CsvGenerators._

  private val generators = new CsvGenerators {}

  implicit val bigIntRenderer: CsvRenderer[BigInt] = new CsvRenderer[BigInt] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(t: BigInt, attrs: Map[String, String]): String = t.toString(16)
  }
  implicit val crimeIdRenderer: CsvRenderer[Option[BigInt]] = optionRenderer[BigInt]("unidentified")
  implicit val crimeIdGenerator: CsvGenerator[Option[BigInt]] = generators.optionGenerator
  implicit val crimeRenderer: CsvProduct[CrimeLocation] = rendererGenerator3(CrimeLocation.apply)
}

