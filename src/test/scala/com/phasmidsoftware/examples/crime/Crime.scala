package com.phasmidsoftware.examples.crime

import com.phasmidsoftware.parse._
import com.phasmidsoftware.render._
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.{EvaluateIO, IOUsing}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
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
 * @param crimeLocation       a CrimeLocation.
 * @param crimeType           see Kaggle.
 * @param lastOutcomeCategory see Kaggle.
 * @param context             see Kaggle.
 */
case class Crime(sequence: Sequence,
                 crimeID: Option[BigInt],
                 month: String,
                 reportedBy: String,
                 fallsWithin: String,
                 crimeLocation: CrimeLocation,
                 crimeType: String,
                 lastOutcomeCategory: String,
                 context: String) extends Sequential {
  def brief: Option[CrimeBrief] = for (long <- crimeLocation.longitude; lat <- crimeLocation.latitude) yield CrimeBrief(crimeID, long, lat)
}

object Crime {
  implicit val crimeOrdering: Ordering[Crime] = Sequential.ordering[Crime]
}

/**
 * CrimeLocation.
 *
 * @param longitude (optional Double) the longitude of the incident.
 * @param latitude  (optional Double) the latitude of the incident.
 * @param location  see Kaggle.
 * @param lsoaCode  see Kaggle.
 * @param lsoaName  see Kaggle.
 */
case class CrimeLocation(longitude: Option[Double],
                         latitude: Option[Double],
                         location: String,
                         lsoaCode: String,
                         lsoaName: String
                        )

case class CrimeBrief(crimeID: Option[BigInt],
                      longitude: Double,
                      latitude: Double) {
}

object CrimeBrief {
  implicit val crimeBriefOrdering: Ordering[CrimeBrief] = NonSequential.optionalOrdering[CrimeBrief, BigInt](c => c.crimeID)
}

object LocationParser extends CellParsers {
  /**
   * Precede each upper case letter (or digit) with _.
   */
  def camelToSnakeCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z\\d])", " $1")

  implicit val locationColumnHelper: ColumnHelper[CrimeLocation] = columnHelper(camelToSnakeCaseColumnNameMapper _,
    "lsoaCode" -> "LSOA code",
    "lsoaName" -> "LSOA name"
  )

  implicit val locationParser: CellParser[CrimeLocation] = cellParser5(CrimeLocation)
}

object LocationRenderer extends CsvRenderers {

  import CsvRenderers._
  import com.phasmidsoftware.render.CsvGenerators._

  private val generators = new CsvGenerators {}
  implicit val geoRenderer: CsvRenderer[Option[Double]] = optionRenderer[Double]()
  implicit val geoGenerator: CsvGenerator[Option[Double]] = generators.optionGenerator[Double]
  implicit val locationRenderer: CsvProduct[CrimeLocation] = rendererGenerator5(CrimeLocation)
}

object CrimeParser extends CellParsers {

  import LocationParser._

  implicit object BigIntCellParser extends SingleCellParser[BigInt] {
    def convertString(w: String): Try[BigInt] = implicitly[Parseable[BigInt]].parse(w, Some("16"))
  }

  implicit val crimeColumnHelper: ColumnHelper[Crime] = columnHelper(camelToSnakeCaseColumnNameMapper _,
    "crimeID" -> "Crime ID")

  implicit val crimeIdParser: CellParser[Option[BigInt]] = cellParserOption[BigInt]
  implicit val crimeParser: CellParser[Crime] = cellParser9(Crime.apply)

  implicit object CrimeConfig extends DefaultRowConfig {
    override val listEnclosure: String = ""
  }

  implicit val parser: StandardRowParser[Crime] = StandardRowParser.create[Crime]

  case class CrimeTableParser(override val forgiving: Boolean, override val predicate: Try[Crime] => Boolean) extends StringTableParser[Table[Crime]] with SelectiveParser[Crime, Table[Crime]] {
    type Row = Crime

    val maybeFixedHeader: Option[Header] = None

    val headerRowsToRead: Int = 1

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    def setForgiving(b: Boolean): TableParser[Table[Crime]] = copy(forgiving = b)

    def setPredicate(p: Try[Crime] => Boolean): TableParser[Table[Crime]] = copy(predicate = p)

    protected def builder(rows: Iterable[Crime], header: Header): Table[Row] = HeadedTable(Content(rows), header)
  }

  implicit object CrimeTableParser extends CrimeTableParser(true, _ => true)
}

object CrimeRenderer extends CsvRenderers {

  import CsvRenderers._
  import LocationRenderer._
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
  implicit val crimeRenderer: CsvProduct[Crime] = rendererGenerator9(Crime.apply)
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
  implicit val crimeRenderer: CsvProduct[CrimeBrief] = rendererGenerator3(CrimeBrief.apply)
}

object Main extends App {

  import CrimeLocationRenderer._
  import CrimeParser._
  import cats.effect.IO

  val crimeFile = "2023-01-metropolitan-street.csv"

  val cti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

  val wi: IO[String] = for {
    ct <- cti
    lt <- IO(ct.mapOptional(m => m.brief).filter(m => m.crimeID.isDefined))
    st <- IO(lt.processRows(c => c.sample(450))) //slice(150, 170))
    w <- st.toCSV
  } yield w

  println(EvaluateIO(wi, Timeout(Span(10, Seconds))))
}