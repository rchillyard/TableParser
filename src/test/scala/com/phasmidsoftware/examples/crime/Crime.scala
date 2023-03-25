package com.phasmidsoftware.examples.crime

import com.phasmidsoftware.examples.crime.CrimeLocation.camelToSnakeCaseColumnNameMapper
import com.phasmidsoftware.parse._
import com.phasmidsoftware.render._
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.{EvaluateIO, IOUsing}
import java.net.URL
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
import scala.util.{Random, Try}

/**
 * Case class to represent a Crime from the Kaggle data set:
 * [[https://www.kaggle.com/datasets/marshuu/crimes-in-uk-2023?select=2023-01-metropolitan-street.csv]]
 *
 * The file under main/resources is an edited version of the Metropolitan Crime Statistics 2023-01 (only the first 5,000 rows)
 *
 * @param maybeCrimeId        (optional BigInt in hexadecimal notation) see Kaggle.
 * @param month               see Kaggle.
 * @param reportedBy          see Kaggle.
 * @param fallsWithin         see Kaggle.
 * @param maybeLocation       a CrimeLocation.
 * @param crimeType           see Kaggle.
 * @param lastOutcomeCategory see Kaggle.
 * @param context             see Kaggle.
 */
case class Crime(sequence: Sequence,
                 maybeCrimeId: Option[BigInt],
                 month: String,
                 reportedBy: String,
                 fallsWithin: String,
                 maybeLocation: Option[CrimeLocation],
                 crimeType: String,
                 lastOutcomeCategory: String,
                 context: String) extends Sequential {
  def isValid: Boolean = maybeCrimeId.isDefined && maybeLocation.exists(_.isValid)

  def brief: Option[CrimeBrief] = for (crimeId <- maybeCrimeId; location <- maybeLocation) yield CrimeBrief(crimeId, location.longitude, location.latitude)
}

/**
 * Companion object to Crime.
 */
object Crime extends CellParsers with CsvRenderers {

  import CsvRenderers.{CsvRendererDouble, CsvRendererString}
  import com.phasmidsoftware.render.CsvGenerators._

  val filename: String = "2023-01-metropolitan-street.csv"
  val crimeTriedResource: Try[URL] = Try(classOf[Crime].getResource(Crime.filename))

  implicit object crimeValidity extends Validity[Crime] {
    def isValid(c: Crime): Boolean = c.isValid
  }

  implicit object BigIntCellParser extends SingleCellParser[BigInt] {
    def convertString(w: String): Try[BigInt] = implicitly[Parseable[BigInt]].parse(w, Some("16"))
  }

  implicit val crimeColumnHelper: ColumnHelper[Crime] = columnHelper(camelToSnakeCaseColumnNameMapper _,
    "maybeCrimeId" -> "Crime ID")

  implicit object CrimeConfig extends DefaultRowConfig {
    override val listEnclosure: String = ""
  }

  implicit val crimeIdParser: CellParser[Option[BigInt]] = cellParserOption[BigInt]
  implicit val crimeLocationParser: CellParser[Option[CrimeLocation]] = cellParserOption[CrimeLocation]
  implicit val crimeParser: CellParser[Crime] = cellParser9(Crime.apply)
  implicit val parser: StandardRowParser[Crime] = StandardRowParser.create[Crime]
  implicit val crimeOrdering: Ordering[Crime] = Sequential.ordering[Crime]

  implicit val crimeIDRenderer: CsvRenderer[BigInt] = new CsvRenderer[BigInt] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(t: BigInt, attrs: Map[String, String]): String = t.toString(16)
  }

  private val generators = new CsvGenerators {}
  implicit val crimeIdRenderer: CsvRenderer[Option[BigInt]] = optionRenderer[BigInt]()
  implicit val crimeIdGenerator: CsvGenerator[Option[BigInt]] = generators.optionGenerator[BigInt]
  implicit val geoRenderer: CsvRenderer[Option[Double]] = optionRenderer[Double]()
  implicit val geoGenerator: CsvGenerator[Option[Double]] = generators.optionGenerator[Double]
  implicit val crimeLocationProduct: CsvProduct[Option[CrimeLocation]] = optionProduct[CrimeLocation]()
  implicit val crimeRenderer: CsvProduct[Crime] = rendererGenerator9(Crime.apply)
  // CONSIDER why doesn't including the implicit object CrimeParser.CrimeTableParser work?
  implicit val p: CrimeParser.CrimeTableParser = new CrimeParser.CrimeTableParser(true, _ => true)

  import cats.effect.IO

  def doMain(triedResource: Try[URL])(implicit random: Random): IO[String] =
    for {
      url <- IO.fromTry(triedResource) // get a URL for the full crime file (there is also a sample available)
      ct <- IOUsing(Try(Source.fromURL(url)))(x => Table.parseSource(x)) // open/close resource  and parse it as a Table[Crime].
      lt <- IO(ct.filterValid.mapOptional(m => m.brief)) // filter according to validity and then convert rows to CrimeBrief.
      st <- IO(lt.sample(450)) // sample 1 in every (approximately) 450 rows.
      w <- st.toCSV // write the table out in CSV format.
    } yield w

}

/**
 * CrimeLocation.
 *
 * @param longitude the longitude of the incident.
 * @param latitude  the latitude of the incident.
 * @param location  see Kaggle.
 * @param lsoaCode  see Kaggle.
 * @param lsoaName  see Kaggle.
 */
case class CrimeLocation(longitude: Double,
                         latitude: Double,
                         location: String,
                         lsoaCode: String,
                         lsoaName: String
                        ) {
  def isValid: Boolean = CrimeLocation.isValid(longitude, latitude, lsoaCode)
}

/**
 * Companion object to CrimeLocation.
 */
object CrimeLocation extends CellParsers with CsvRenderers {
  private val invalidLSOACodes = Seq("E01032496", "E01011349", "E01024436", "E01032969", "E01021416", "E01021427", "E01016619", "E01015693", "E01032731", "E01030261", "E01023724", "E01023548", "E01009385", "E01016920", "E01000387", "E01026188", "E01030384", "E01017765", "E01031789", "E01003802", "E01016215", "E01010676", "E01024821", "E01000755", "E01000686", "E01027148", "E01033022", "E01028101", "E01024261", "E01016608", "E01030606", "E01016464", "E01023805", "E01009923", "E01033451", "E01001126", "E01030300", "E01021765", "E01010326", "E01024172", "E01015772", "E01021945", "E01000833", "E01010054", "E01031587", "E01005692", "E01023302", "E01010635", "E01002255", "E01030333", "E01024475", "E01033212", "E01016006", "E01002922", "E01006386", "E01032645", "E01033739", "E01015982", "E01030668", "E01016540", "E01018996", "E01021818", "E01024429", "E01002288", "E01016074", "E01002462", "E01003466", "E01023951", "E01020995", "E01030350", "E01015935", "E01023344", "E01024243", "E01017810", "E01017392", "E01003846", "E01030851", "E01033542", "E01015992", "E01023793", "E01023840", "E01030548", "E01004707", "E01024247", "E01003008", "E01001107", "E01032979", "E01016129", "E01023963", "E01023778", "E01024189", "E01031333", "E01030685", "E01005197", "E01032799", "E01021749", "E01000345", "E01023580", "E01030306", "E01023850", "E01030743", "E01002359", "E01023849", "E01030751", "E01008709", "E01006832", "E01024155", "E01023861", "E01023908", "E01023644", "E01024185", "E01002995", "E01017811", "E01030323", "E01023341", "E01023649", "E01030704", "E01030856", "E01025277", "E01021954", "E01025627", "E01032684", "E01000356", "E01006194", "E01022295", "E01032571", "E01013916", "E01023573", "E01030392", "E01024152", "E01003138", "E01005568", "E01024149", "E01004338", "E01017619", "E01023942", "E01021310", "W01000010", "E01023378", "E01015688", "E01000425", "E01021663", "E01023444", "E01032378", "E01030933", "E01024047", "E01017989", "E01017423", "E01011036", "E01010425", "E01030201", "E01025767", "E01030735", "E01021436", "E01021447", "E01015777", "E01027711", "E01000717", "E01030610", "E01000436", "E01000836", "E01021806", "E01000371", "E01030855", "E01023352", "E01026959", "E01020971", "E01021319", "E01004097", "E01015734", "E01028660", "E01009709", "E01015241", "E01001058", "E01024162", "E01000461", "E01024745", "E01013665", "E01016474", "E01010813", "E01026591", "E01030566", "E01024186", "E01012454", "E01029475", "E01015782", "E01014706", "E01000003", "E01030531", "E01016011", "E01023541", "E01024783", "E01016098", "E01023844", "E01001069", "E01031819", "E01016939", "E01024169", "E01033747", "W01001867", "W01000733", "E01021469", "E01023877", "E01003457", "E01020507", "E01016912", "E01030717", "E01028843", "E01004163", "E01021324", "E01026868", "E01024420", "E01024158", "E01016482", "E01021500", "E01023758", "E01033742", "E01000949", "E01020086", "E01015808", "E01024136", "E01000932", "E01016034", "E01017155", "E01001456", "E01023842", "E01016549", "E01002388", "E01008551", "E01030344", "E01005798", "E01028331", "E01017812", "E01023339", "E01030310", "E01002155", "E01023899", "E01017139", "E01033135", "E01025802", "E01002699", "E01006211", "E01016602", "E01015773", "E01018219", "E01033164", "E01003676", "E01030853", "E01033345", "E01015902", "E01016247", "E01004475", "E01015951", "E01003691", "E01001350", "E01015795", "E01006633", "E01023559", "E01027320", "E01014073", "E01016385", "E01016450", "E01030755", "E01000723", "E01030744", "E01013258", "E01023913", "E01024391", "E01031723", "E01001236", "E01011992")

  def isValid(longitude: Double, latitude: Double, lsoaCode: String): Boolean =
    !(latitude > 51.7 || longitude > 0.3 || latitude < 51.2 || longitude < -0.51 || invalidLSOACodes.contains(lsoaCode))

  /**
   * Precede each upper case letter (or digit) with _.
   */
  def camelToSnakeCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z\\d])", " $1")

  implicit val locationColumnHelper: ColumnHelper[CrimeLocation] = columnHelper(camelToSnakeCaseColumnNameMapper _,
    "lsoaCode" -> "LSOA code",
    "lsoaName" -> "LSOA name"
  )

  implicit val locationParser: CellParser[CrimeLocation] = cellParser5(CrimeLocation.apply)

  import CsvGenerators._
  import CsvRenderers._

  implicit val locationRenderer: CsvProduct[CrimeLocation] = rendererGenerator5(CrimeLocation.apply)
}

/**
 * Case class to model a very brief version of a crime (only the ID, and geographic location).
 * This is for the INFO6205 project, Spring 2023, solving the TSP.
 *
 * @param crimeID   (BigInt) the crime ID of the incident, expressed in CSV in hexadecimal.
 * @param longitude (Double) the longitude of the incident.
 * @param latitude  (Double) the latitude of the incident.
 */
case class CrimeBrief(crimeID: BigInt,
                      longitude: Double,
                      latitude: Double) {
}

object CrimeBrief extends CsvRenderers {
  implicit val crimeBriefOrdering: Ordering[CrimeBrief] = NonSequential.ordering[CrimeBrief, BigInt](c => c.crimeID)

  import Crime.crimeIDRenderer
  import CsvRenderers.CsvRendererDouble
  import com.phasmidsoftware.render.CsvGenerators._

  private val generators = new CsvGenerators {}
  implicit val crimeIdRenderer: CsvRenderer[Option[BigInt]] = optionRenderer[BigInt]("unidentified")
  implicit val crimeIdGenerator: CsvGenerator[Option[BigInt]] = generators.optionGenerator
  implicit val crimeRenderer: CsvProduct[CrimeBrief] = rendererGenerator3(CrimeBrief.apply)
}

object CrimeParser extends CellParsers {
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

/**
 * Main program to create a sample of valid rows from the complete Metropolitan crime dataset.
 */
object Main extends App {

  import cats.effect.IO

  implicit val random: Random = new Random()
  val wi: IO[String] = Crime.doMain(Crime.crimeTriedResource)

  println(EvaluateIO(wi, Timeout(Span(10, Seconds))))
}