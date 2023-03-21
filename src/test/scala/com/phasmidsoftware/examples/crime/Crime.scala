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
  def isValid: Boolean = crimeID.isDefined && crimeLocation.isValid

  def brief: Option[CrimeBrief] = for (long <- crimeLocation.longitude; lat <- crimeLocation.latitude) yield CrimeBrief(crimeID, long, lat)
}

object Crime {
  implicit object crimeValidity extends Validity[Crime] {
    def isValid(c: Crime): Boolean = c.isValid
  }

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
                        ) {
  def isValid: Boolean = (longitude, latitude) match {
    case (Some(long), Some(lat)) => CrimeLocation.isValid(long, lat, lsoaCode)
    case _ => false
  }

}

object CrimeLocation {
  private val invalidLSOACodes = Seq("E01032496", "E01011349", "E01024436", "E01032969", "E01021416", "E01021427", "E01016619", "E01015693", "E01032731", "E01030261", "E01023724", "E01023548", "E01009385", "E01016920", "E01000387", "E01026188", "E01030384", "E01017765", "E01031789", "E01003802", "E01016215", "E01010676", "E01024821", "E01000755", "E01000686", "E01027148", "E01033022", "E01028101", "E01024261", "E01016608", "E01030606", "E01016464", "E01023805", "E01009923", "E01033451", "E01001126", "E01030300", "E01021765", "E01010326", "E01024172", "E01015772", "E01021945", "E01000833", "E01010054", "E01031587", "E01005692", "E01023302", "E01010635", "E01002255", "E01030333", "E01024475", "E01033212", "E01016006", "E01002922", "E01006386", "E01032645", "E01033739", "E01015982", "E01030668", "E01016540", "E01018996", "E01021818", "E01024429", "E01002288", "E01016074", "E01002462", "E01003466", "E01023951", "E01020995", "E01030350", "E01015935", "E01023344", "E01024243", "E01017810", "E01017392", "E01003846", "E01030851", "E01033542", "E01015992", "E01023793", "E01023840", "E01030548", "E01004707", "E01024247", "E01003008", "E01001107", "E01032979", "E01016129", "E01023963", "E01023778", "E01024189", "E01031333", "E01030685", "E01005197", "E01032799", "E01021749", "E01000345", "E01023580", "E01030306", "E01023850", "E01030743", "E01002359", "E01023849", "E01030751", "E01008709", "E01006832", "E01024155", "E01023861", "E01023908", "E01023644", "E01024185", "E01002995", "E01017811", "E01030323", "E01023341", "E01023649", "E01030704", "E01030856", "E01025277", "E01021954", "E01025627", "E01032684", "E01000356", "E01006194", "E01022295", "E01032571", "E01013916", "E01023573", "E01030392", "E01024152", "E01003138", "E01005568", "E01024149", "E01004338", "E01017619", "E01023942", "E01021310", "W01000010", "E01023378", "E01015688", "E01000425", "E01021663", "E01023444", "E01032378", "E01030933", "E01024047", "E01017989", "E01017423", "E01011036", "E01010425", "E01030201", "E01025767", "E01030735", "E01021436", "E01021447", "E01015777", "E01027711", "E01000717", "E01030610", "E01000436", "E01000836", "E01021806", "E01000371", "E01030855", "E01023352", "E01026959", "E01020971", "E01021319", "E01004097", "E01015734", "E01028660", "E01009709", "E01015241", "E01001058", "E01024162", "E01000461", "E01024745", "E01013665", "E01016474", "E01010813", "E01026591", "E01030566", "E01024186", "E01012454", "E01029475", "E01015782", "E01014706", "E01000003", "E01030531", "E01016011", "E01023541", "E01024783", "E01016098", "E01023844", "E01001069", "E01031819", "E01016939", "E01024169", "E01033747", "W01001867", "W01000733", "E01021469", "E01023877", "E01003457", "E01020507", "E01016912", "E01030717", "E01028843", "E01004163", "E01021324", "E01026868", "E01024420", "E01024158", "E01016482", "E01021500", "E01023758", "E01033742", "E01000949", "E01020086", "E01015808", "E01024136", "E01000932", "E01016034", "E01017155", "E01001456", "E01023842", "E01016549", "E01002388", "E01008551", "E01030344", "E01005798", "E01028331", "E01017812", "E01023339", "E01030310", "E01002155", "E01023899", "E01017139", "E01033135", "E01025802", "E01002699", "E01006211", "E01016602", "E01015773", "E01018219", "E01033164", "E01003676", "E01030853", "E01033345", "E01015902", "E01016247", "E01004475", "E01015951", "E01003691", "E01001350", "E01015795", "E01006633", "E01023559", "E01027320", "E01014073", "E01016385", "E01016450", "E01030755", "E01000723", "E01030744", "E01013258", "E01023913", "E01024391", "E01031723", "E01001236", "E01011992")

  def isValid(longitude: Double, latitude: Double, lsoaCode: String): Boolean =
    !(latitude > 51.7 || longitude > 0.3 || latitude < 51.2 || longitude < -0.51 || invalidLSOACodes.contains(lsoaCode))

}
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

  implicit val locationParser: CellParser[CrimeLocation] = cellParser5(CrimeLocation.apply)
}

object LocationRenderer extends CsvRenderers {

  import CsvRenderers._
  import com.phasmidsoftware.render.CsvGenerators._

  private val generators = new CsvGenerators {}
  implicit val geoRenderer: CsvRenderer[Option[Double]] = optionRenderer[Double]()
  implicit val geoGenerator: CsvGenerator[Option[Double]] = generators.optionGenerator[Double]
  implicit val locationRenderer: CsvProduct[CrimeLocation] = rendererGenerator5(CrimeLocation.apply)
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
    lt <- IO(ct.filterValid.mapOptional(m => m.brief).filter(m => m.crimeID.isDefined))
    st <- IO(lt.processRows(c => c.sample(450))) //slice(150, 170))
    w <- st.toCSV
  } yield w

  println(EvaluateIO(wi, Timeout(Span(10, Seconds))))
}