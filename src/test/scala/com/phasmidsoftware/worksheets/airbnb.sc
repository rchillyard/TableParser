import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.FP.resource

import scala.io.Source
import scala.util.{Success, Try}

// NOTE: We show how to parse the AirBNB dataset where the resulting Table rows.
// are plain sequences of String (no parsing to specific types).
// We also show how to analyze the resulting columns.
// This is useful for unfamiliar datasets.

// NOTE: Set up the source.
// The resource method is just a utility for convenience--
// you could just as easily use the Java URL methods directly.
// The result (sy) is a Try[Source].
val sy = for (u <- resource[AirBNBSpec]("/airbnb2.csv")) yield Source.fromURL(u)

// NOTE: Set up the parser as a "raw" parser (no conversion to types).
// We set multiline to be true because the AirBNB file has many lines which "run on"
// where a quoted string includes a newline character.
// We set the predicate only for demonstration purposes--here we randomly sample one in every two rows.
// The resulting parser is a RawTableParser which in turn is a TableParser[Table[Seq[String]]].
val parser: RawTableParser = RawTableParser().setMultiline(true).setPredicate(TableParser.sampler(2))

// NOTE: parse the source to create the table.
// This triggers usage of an implicit class (in the TableParser companion object) which defines several parse methods.
// The result is a Try[RawTable].
val wsty: Try[RawTable] = parser parse sy

// NOTE: if successful, analyze the resulting rows and print the analysis.
// There should be 87 columns and approximately 128 rows in this resulting table (the exact number is random).
// Then print the first 10 rows.
wsty match {
  case Success(t@HeadedTable(r, _)) =>
    val analysis = Analysis(t)
    println(s"AirBNB: $analysis")
    r take 10 foreach println
  case _ =>
}
