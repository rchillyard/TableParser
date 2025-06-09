/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.examples

import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table.{CsvAttributes, HeadedTable, Header, Table}
import scala.util.Try

/**
 * This class represents a Movie from the IMDB data file on Kaggle.
 * Although the limitation on 22 fields in a case class has partially gone away, it's still convenient to group the different attributes together into logical classes.
 *
 * Created by scalaprof on 9/12/16.
 *
 * Common questions in this assignment:
 * 1. Where is main method?
 * In most case, you don't need to run main method for assignments.
 * Unit tests are provided to test your implementation.
 * In this assignment, you will find the `object Movie extends App`,
 * the `App` trait can be used to quickly turn objects into executable programs.
 * You can read the official doc of Scala for more details.
 *
 * 2. How to understand the whole program in this assignment?
 * I won't suggest you to understand the whole program in this assignment,
 * there are some advanced features like `implicit` which hasn't been covered in class.
 * You should be able to understand it before midterm.
 * I will suggest you only focus on each TO BE IMPLEMENTED in the assignments.
 *
 */
case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Option[Principal], genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

/**
 * The movie format (including language and duration).
 *
 * @param color       whether filmed in color
 * @param language    the native language of the characters
 * @param aspectRatio the aspect ratio of the film (optional)
 * @param duration    its length in minutes (optional)
 */
case class Format(color: String, language: String, aspectRatio: Option[Double], duration: Option[Int]) {
  override def toString: String = {
    s"$color,$language,$aspectRatio,$duration"
  }
}

/**
 * The production: its country, year, and financials
 *
 * @param country   country of origin
 * @param budget    (optional) production budget in US dollars
 * @param gross     (optional) gross earnings (?)
 * @param titleYear the year the title was registered (?)
 */
case class Production(country: String, budget: Option[Int], gross: Option[Int], titleYear: Option[Int]) {
  def isKiwi: Boolean = this match {
    case Production("New Zealand", _, _, _) => true
    case _ => false
  }
}

/**
 * Information about various forms of review, including the content rating.
 */
case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Option[Int], numUsersVoted: Int, numCriticReviews: Option[Int], totalFacebookLikes: Int)

/**
 * A cast or crew principal
 *
 * @param name          name
 * @param facebookLikes number of FaceBook likes
 */
case class Principal(name: Name, facebookLikes: Int) {
  override def toString = s"$name ($facebookLikes likes)"
}

/**
 * A name of a contributor to the production
 *
 * @param first  first name
 * @param middle middle name or initial
 * @param last   last name
 * @param suffix suffix
 */
case class Name(first: String, middle: Option[String], last: String, suffix: Option[String]) {
  override def toString: String = {
    case class Result(r: StringBuffer) {
      def append(s: String): Unit = r.append(" " + s)

      override def toString: String = r.toString
    }
    val r: Result = Result(new StringBuffer(first))
    middle foreach {
      r.append
    }
    r.append(last)
    suffix foreach {
      r.append
    }
    r.toString
  }
}

/**
 * The US rating.
 * NOTE: this definition does not cover all of the ratings in the IMDB movie dataset.
 * That's OK--this is just an exemplar.
 */
case class Rating(code: String, age: Option[Int]) {
  override def toString: String = code + (age match {
    case Some(x) => "-" + x
    case _ => ""
  })
}

/**
 * Object `MovieParser` provides utility functions and implicits for parsing movie-related data.
 * It extends the `CellParsers` trait and defines parsing configurations, helpers, and processors
 * for handling various components such as movies, reviews, production details, and more.
 */
object MovieParser extends CellParsers {

  /**
   * Precede each upper case letter (or digit) with _.
   */
  val camelToSnakeCaseColumnNameMapper: String => String = _.replaceAll("([A-Z\\d])", "_$1")

  implicit val movieColumnHelper: ColumnHelper[Movie] = columnHelper(camelToSnakeCaseColumnNameMapper,
    "title" -> "movie_title",
    "imdb" -> "movie_imdb_link")
  implicit val reviewsColumnHelper: ColumnHelper[Reviews] = columnHelper(camelToSnakeCaseColumnNameMapper,
    "facebookLikes" -> "movie_facebook_likes",
    "numUsersReview" -> "num_user_for_reviews",
    "numUsersVoted" -> "num_voted_users",
    "numCriticReviews" -> "num_critic_for_reviews",
    "totalFacebookLikes" -> "cast_total_facebook_likes")
  implicit val formatColumnHelper: ColumnHelper[Format] = columnHelper(camelToSnakeCaseColumnNameMapper)
  implicit val productionColumnHelper: ColumnHelper[Production] = columnHelper(camelToSnakeCaseColumnNameMapper)
  implicit val principalColumnHelper: ColumnHelper[Principal] = columnHelper(camelToSnakeCaseColumnNameMapper, Some("$x_$c"))
  implicit val ratingParser: CellParser[Rating] = cellParser(Rating.apply: String => Rating)
  implicit val formatParser: CellParser[Format] = cellParser4(Format)
  implicit val productionParser: CellParser[Production] = cellParser4(Production)
  implicit val nameParser: CellParser[Name] = cellParser(Name.apply)
  implicit val principalParser: CellParser[Principal] = cellParser2(Principal)
  implicit val reviewsParser: CellParser[Reviews] = cellParser7(Reviews)
  implicit val attributesParser: CellParser[AttributeSet] = cellParser(AttributeSet.apply: String => AttributeSet)
  implicit val optionalPrincipalParser: CellParser[Option[Principal]] = cellParserOption
  implicit val movieParser: CellParser[Movie] = cellParser11(Movie.apply)

  /**
   * An implicit object extending `DefaultRowConfig`, used for customizing the configuration settings
   * for parsing rows in the context of the `MovieParser` class.
   *
   * The `MovieConfig` object overrides the default `listEnclosure` property to use an empty string (`""`)
   * instead of the default `{}`. This customization may imply that no special characters are used to
   * enclose lists in this parsing configuration.
   *
   * @see DefaultRowConfig
   */
  implicit object MovieConfig extends DefaultRowConfig {
    override val listEnclosure: String = ""
  }

  implicit val parser: StandardRowParser[Movie] = StandardRowParser.create[Movie]

  /**
   * The `MovieTableParser` trait provides functionality for parsing movie information from a string-based table representation
   * into a structured `Table` of `Movie` rows. It is a specialized parser built on top of the `StringTableParser` abstraction.
   *
   * Key Features:
   * - **Row Type (`Row`)**: Defines the row type as `Movie`, indicating that each parsed row corresponds to a `Movie` entity.
   * - **Forgiving Parsing**: Operates in a forgiving mode (`forgiving = true`), allowing certain non-critical parsing errors to be bypassed.
   * - **Header Handling**:
   *   - Supports an optional fixed header (`maybeFixedHeader`), which is `None` by default, indicating that headers are dynamically parsed.
   *   - Configures parsing to read `headerRowsToRead = 1` row for table headers by default.
   *     - **Row Parsing**: Utilizes an implicit `RowParser` to transform string data into `Movie` instances, enabling flexible and type-safe parsing.
   *     - **Table Construction**: Uses the `builder` method to combine rows and a parsed header into a `Table`. The resulting table is typically a `HeadedTable`
   *       type containing the parsed rows and associated header metadata.
   *
   * ## Fields:
   * - `maybeFixedHeader`: An optional value representing a fixed header. If `None`, the header is inferred from the input table.
   * - `headerRowsToRead`: The number of rows to read from the input as the header. Defaults to `1`.
   * - `forgiving`: A flag indicating whether to allow lenient parsing. Defaults to `true`.
   * - `rowParser`: An implicit `RowParser` instance responsible for converting string data into `Movie` objects.
   *
   * ## Key Methods:
   * - `builder`: Combines parsed rows and an inferred or fixed header to produce a `Table[Movie]`.
   *
   * ## Usage:
   * This trait is typically mixed into concrete implementations or utilized via an implicit object to parse tables of movie
   * information. It is used in scenarios where a string-based representation of a table needs to be parsed into a structured
   * representation of `Movie` rows along with header metadata.
   *
   * Example:
   * ```
   * val tableString: Iterator[String] = Iterator(
   * "Title,Director,Year,Genre",
   * "Inception,Christopher Nolan,2010,Sci-Fi",
   * "The Godfather,Francis Ford Coppola,1972,Crime"
   * )
   * val parsedTable: Try[Table[Movie]] = implicitly[MovieTableParser].parse(tableString, 1)
   * ```
   *
   * In this example, the `MovieTableParser` is implicitly used to parse a table of movies into
   * a structured `Table[Movie]` containing row and header information.
   *
   * ## Notes:
   * - The `MovieTableParser` works seamlessly with other parsing utilities provided by the `StringTableParser` framework.
   * - The `builder` method can be overridden in custom implementations for advanced use cases or alternate table construction logic.
   *
   * @see `StringTableParser`
   * @tparam Table the resulting table type, which in this case is `Table[Movie]`.
   */
  trait MovieTableParser extends StringTableParser[Table[Movie]] {
    type Row = Movie

    val maybeFixedHeader: Option[Header] = None

    val headerRowsToRead: Int = 1

    override val forgiving: Boolean = true

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    protected def builder(rows: Iterable[Movie], header: Header): Table[Row] = HeadedTable(rows, header)
  }

  /**
   * Companion object `MovieTableParser` acts as an implicit implementation of `MovieTableParser` trait.
   *
   * The `MovieTableParser` is responsible for parsing a string representation of a table
   * to produce a structured `Table` of `Movie` rows. It extends the `StringTableParser` trait
   * with type `Table[Movie]`, leveraging predefined parsing rules and configurations.
   *
   * Key Characteristics:
   * - Defines `Row` as `Movie` type, signifying that each parsed row corresponds to a `Movie` entity.
   * - Supports an optional fixed header via `maybeFixedHeader`, which is set to `None` by default.
   * - Reads and interprets a single header row (`headerRowsToRead = 1`) unless overridden.
   * - Operates in a `forgiving` mode (`forgiving = true`), allowing certain parsing errors to be bypassed gracefully.
   * - Uses an implicit `RowParser` to transform individual row data into `Movie` objects.
   * - Combines rows and headers into a `Table` using the method `builder`, which constructs a `HeadedTable` containing rows and the header.
   *
   * This object is typically utilized in contexts where implicit resolution is required to enable
   * seamless parsing of movie-related tables using compatible APIs or libraries.
   */
  implicit object MovieTableParser extends MovieTableParser

  /**
   * The `MovieRowProcessor` trait provides a concrete implementation for processing rows of
   * raw input (as Strings) representing `Movie` objects. This trait extends the `StringRowProcessor`
   * tailored for `Movie` rows, deducing `String` as the input type and `Movie` as the row type.
   *
   * Specifically, this trait:
   * - Adopts a fault-tolerant approach to parsing by setting `forgiving = true`, enabling the
   * processor to recover from minor parsing errors where possible.
   * - Utilizes an implicit `RowParser[Movie, String]` to handle the actual parsing logic for individual rows.
   * - Optionally relies on a `Header` for structured row processing. If a fixed header is not provided
   * via `maybeFixedHeader`, additional rows can be consumed to dynamically determine the header.
   * - Sets the default number of header rows to read as 1 (`headerRowsToRead`).
   *
   * This processor is an integral part of the parsing workflow for `Movie` data parsed from
   * input sources such as files or streams.
   *
   * Key Type Members and Fields:
   * - `Row`: Alias for `Movie` as the row type this processor handles.
   * - `maybeFixedHeader`: Optionally allows for a pre-determined table header for consistent column names.
   * - `headerRowsToRead`: Specifies the number of rows to consume when detecting a dynamic header.
   * - `forgiving`: Configures fault-tolerance for processing, set to `true` by default.
   * - `rowParser`: An implicit instance of `RowParser */
  trait MovieRowProcessor extends StringRowProcessor[Movie] {
    type Row = Movie

    val maybeFixedHeader: Option[Header] = None

    val headerRowsToRead: Int = 1

    override val forgiving: Boolean = true

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

  }

  /**
   * `MovieRowProcessor` is an implicit object that extends the `MovieRowProcessor` trait, providing
   * a concrete implementation for parsing and processing rows of input data (as Strings) into
   * `Movie` objects. It leverages the functionality defined in the `StringRowProcessor` for
   * handling string-input row processing with specific configurations tailored for movie data.
   *
   * Key Features:
   * - Parses rows of input data into `Movie` objects using an implicit `RowParser[Movie, String]`.
   * - Supports fault-tolerant parsing through the `forgiving` attribute, allowing recovery from
   * minor errors during row processing.
   * - Can work with both fixed headers and dynamically detected headers from the input data. The
   * number of rows to read for the dynamic header is set to 1 by default (`headerRowsToRead`).
   *
   * Use Case:
   * This processor is commonly used in workflows for parsing and transforming movie data from
   * input formats such as CSV files or text streams into structured `Movie` objects, allowing
   * further processing or analysis.
   *
   * @see `StringRowProcessor` for base processing functionality.
   * @see `RowParser` for parsing logic details.
   * @see `Movie` for the detailed representation of the processed row data.
   */
  implicit object MovieRowProcessor extends MovieRowProcessor
}

/**
 * Companion object for the `Name` case class, providing factory methods for parsing strings into `Name` instances.
 *
 * This object handles the parsing of a formatted name string (e.g., "John A. Smith Jr.") into its respective
 * components: first name, middle name/initial, last name, and optional suffix. It uses a regular expression
 * to perform this parsing and validates the input.
 *
 * ==Regex Pattern==
 * The internal regular expression (`rName`) is designed to match and extract components of a name. While it
 * works for most typical name formats, some complex or unconventional names in certain datasets (e.g., the Movie
 * database) might not be parsed correctly. The components matched by the regex are:
 *  - First Name (mandatory)
 *  - Middle Name/Initial (optional)
 *  - Last Name (mandatory)
 *  - Suffix (optional)
 *
 * ==Factory Methods==
 * The apply method is overloaded to:
 *  - Parse a single string into a `Name` instance.
 *  - Throw an exception if the input string does not conform to the expected format.
 *
 * @example
 * {{{
 * val name1 = Name("John Smith")             // Name("John", None, "Smith", None)
 * val name2 = Name("John A. Smith")          // Name("John", Some("A."), "Smith", None)
 * val name3 = Name("John Smith Jr.")         // Name("John", None, "Smith", Some("Jr."))
 * val name4 = Name("John A. Smith Jr.")      // Name("John", Some("A."), "Smith", Some("Jr."))
 *
 * // An invalid name format will throw an exception
 * val invalidName = Name("John")  // throws Exception: parse error in Name: 'John'
 * }}}
 *
 * ==Usage in Context==
 * The `Name` class and object are primarily used in parsing movie-related data structures and rendering them to/from CSVs:
 *  - The `createMovieCvsGenerator` method uses `Name.apply` to generate CSV entries for `Name` instances.
 *  - Rendering movie-related data via `CsvRendererMovie` involves the `rendererName` generator, built around `Name`.
 * @note The `Name` case class has a corresponding custom `toString` implementation for formatted string representation.
 */
object Name {
  // NOTE: this regex will not parse all names in the Movie database correctly. Still, it gets most of them.
  private val rName =
    """^([\p{L}\-']+\.?)\s*(([\p{L}\-]+\.?)\s)?([\p{L}\-']+\.?)(\s([\p{L}\-]+\.?))?$""".r

  def apply(name: String): Name = name match {
    case rName(first, _, null, last, _, null) =>
      apply(first, None, last, None)
    case rName(first, _, middle, last, _, null) =>
      apply(first, Some(middle), last, None)
    case rName(first, _, null, last, _, suffix) =>
      apply(first, None, last, Some(suffix))
    case rName(first, _, middle, last, _, suffix) =>
      apply(first, Some(middle), last, Some(suffix))
    case _ =>
      throw new Exception(s"""parse error in Name: '$name'""")
  }
}

/**
 * Companion object for the `Rating` case class.
 * This object provides utility methods to construct `Rating` instances and to parse rating strings into `Rating`.
 */
object Rating {
  /**
   * Alternative apply method for the Rating class such that a single String is decoded
   *
   * @param s a String made up of a code, optionally followed by a dash and a number, e.g. "R" or "PG-13"
   * @return a Rating
   */
  def apply(s: String): Rating =
    s match {
      case rRating(code, _, null) =>
        apply(code, None)
      case rRating(code, _, age) =>
        apply(code, Try(age.toInt).toOption)
      case _ =>
        throw new Exception(s"""parse error in Rating: '$s'""")
    }

  private val rRating = """^(\w*)(-(\d\d))?$""".r
}

/**
 * The `Movie` object provides functionality for generating CSV representations of `Movie` instances.
 * It utilizes the `CsvGenerators` framework to define and create CSV generators for the various fields
 * and nested components of the `Movie` class, as well as the `Movie` class itself.
 *
 * ==Overview==
 * The `Movie` object contains the following key elements:
 *
 * - **csvGenerators**: An instance of `CsvGenerators` that provides predefined CSV generators for standard data types
 * such as `Boolean`, `Int`, `Double`, and `String`. It serves as the foundation for custom CSV generators.
 *
 * - **createMovieCvsGenerator**: A method that constructs a `CsvGenerator` specific to the `Movie` class. This generator
 * supports serialization of `Movie` instances into CSV format, accounting for nested data structures and optional values.
 *
 * ==Dependencies==
 * This object imports the following from `com.phasmidsoftware.tableparser.core.render.CsvGenerators`:
 * - Predefined implicit CSV generators for basic data types and utilities for creating additional generators.
 *
 * ==Key Method Descriptions==
 *
 * - `createMovieCvsGenerator`:
 *   - Constructs and returns a `CsvGenerator[Movie]`, facilitating the generation of CSV rows for `Movie` objects.
 *   - Custom generators are defined for various components of the `Movie` class, including:
 *     - `StringList`: A sequence of strings.
 *     - `Option[Double]`: Optional `Double` values.
 *     - `Option[Int]`: Optional `Int` values.
 *     - `Option[String]`: Optional `String` values.
 *     - `Format`: A case class representing the format of a `Movie`.
 *     - `Production`: A case class holding details about a `Movie`'s production.
 *     - `Rating`: Represents a case class for the rating information of a `Movie`.
 *     - `Reviews`: A case class containing detailed review information.
 *     - `Name`: A case class for principal names.
 *     - `Principal`: A case class for individuals associated with a `Movie`.
 *     - `Option[Principal]`: An optional `Principal`.
 *     - `AttributeSet`: A case class for sets of attributes such as genres or plot keywords.
 *   - The resulting `CsvGenerator[Movie]` aggregates these component-level generators to enable CSV serialization of the entire `Movie` object.
 *
 * ==Usage==
 * To generate a CSV representation of `Movie` objects:
 * 1. Invoke the `createMovieCvsGenerator` method within this object to obtain the appropriate `CsvGenerator[Movie]`.
 * 2. Use the generated `CsvGenerator` to serialize a `Movie` instance into a CSV format.
 *
 * This utility is especially useful for exporting `Movie` data into a structured format, such as for persistence
 * or integration with external systems that require CSV data.
 *
 * ==Example==
 * {{{
 *   val movieCsvGenerator = Movie.createMovieCvsGenerator
 *   val movie = Movie("Inception", Format(...), Production(...), Reviews(...), ...)
 *   val csvRepresentation = movieCsvGenerator.generate(movie)
 * }}}
 */
object Movie {

  import com.phasmidsoftware.tableparser.core.render.CsvGenerators._

  val csvGenerators: CsvGenerators = new CsvGenerators {}

  /**
   * Creates an instance of `CsvGenerator` for the `Movie` type, enabling the generation of CSV headers for
   * objects of type `Movie`. This method establishes implicit `CsvGenerator` instances for various supporting
   * types, such as `StringList`, `Option[Double]`, and custom classes like `Format`, `Production`, and `Reviews`,
   * as well as nested types like `Principal` and `AttributeSet`.
   *
   * TESTME it appears that this is never referenced
   *
   * These implicit instances are utilized to handle the fields of the `Movie` class, which include primitive fields,
   * complex types, optional values, and sequences, constructing a generator for the composite `Movie` objects.
   * It ensures that all necessary implicit generators are available for the `generator11` method, which can map
   * a case class with eleven parameters (as in `Movie`), using a function corresponding to the apply method of
   * the `Movie` case class.
   *
   * @return a `CsvGenerator[Movie]` capable of generating the column headers required to represent a `Movie` object in CSV format.
   */
  def createMovieCvsGenerator: CsvGenerator[Movie] = {
    implicit val generatorStringList: CsvGenerator[StringList] = csvGenerators.sequenceGenerator[String]
    implicit val generatorOptionDouble: CsvGenerator[Option[Double]] = csvGenerators.optionGenerator
    implicit val generatorOptionInt: CsvGenerator[Option[Int]] = csvGenerators.optionGenerator
    implicit val generatorOptionString: CsvGenerator[Option[String]] = csvGenerators.optionGenerator
    implicit val generatorFormat: CsvGenerator[Format] = csvGenerators.generator4(Format)
    implicit val generatorProduction: CsvGenerator[Production] = csvGenerators.generator4(Production)
    implicit val generatorRating: CsvGenerator[Rating] = csvGenerators.generator2(Rating.apply)
    implicit val generatorReviews: CsvGenerator[Reviews] = csvGenerators.generator7(Reviews)
    implicit val generatorName: CsvGenerator[Name] = csvGenerators.generator4(Name.apply)
    implicit val generatorPrincipal: CsvGenerator[Principal] = csvGenerators.generator2(Principal)
    implicit val generatorOptionPrincipal: CsvGenerator[Option[Principal]] = csvGenerators.optionGenerator
    val fAttributeSet: StringList => AttributeSet = AttributeSet.apply
    implicit val generatorAttributeSet: CsvGenerator[AttributeSet] = csvGenerators.generator1(fAttributeSet)
    csvGenerators.generator11(Movie.apply)
  }
}

/**
 * A CSV renderer for the `Movie` class that provides functionality to convert `Movie` instances into CSV format
 * using configurable attributes such as delimiters and quotes.
 *
 * The `CsvRendererMovie` class leverages implicit instances of `CsvRenderer` and `CsvGenerator`
 * for various nested and auxiliary types within a `Movie`.
 * It serves as a specialized renderer for handling complex `Movie` objects with its associated subcomponents.
 *
 * Requires an implicit instance of `CsvAttributes` to define how the CSV output is formatted.
 * CONSIDER removing the csvAttributes parameter and making it an object.
 *
 * @constructor Initializes a `CsvRendererMovie` with the provided CSV attributes.
 * @param csvAttributes Implicit instance of `CsvAttributes` providing delimiters and quote characters for CSV rendering.
 */
class CsvRendererMovie(implicit val csvAttributes: CsvAttributes) extends CsvRenderers with CsvRenderer[Movie] {

  import com.phasmidsoftware.tableparser.core.render.CsvGenerators._

  private val csvGenerators = new CsvGenerators {}
  implicit val generatorStringList: CsvGenerator[StringList] = csvGenerators.sequenceGenerator[String]
  implicit val generatorOptionDouble: CsvGenerator[Option[Double]] = csvGenerators.optionGenerator
  implicit val generatorOptionInt: CsvGenerator[Option[Int]] = csvGenerators.optionGenerator
  implicit val generatorOptionString: CsvGenerator[Option[String]] = csvGenerators.optionGenerator

  import com.phasmidsoftware.tableparser.core.render.CsvRenderers._

  implicit val rendererStringList: CsvRenderer[StringList] = sequenceRenderer[String]
  implicit val rendererOptionDouble: CsvRenderer[Option[Double]] = optionRenderer()
  implicit val rendererOptionInt: CsvRenderer[Option[Int]] = optionRenderer()
  implicit val rendererOptionString: CsvRenderer[Option[String]] = optionRenderer()
  implicit val rendererFormat: CsvProduct[Format] = rendererGenerator4(Format)
  implicit val rendererProduction: CsvRenderer[Production] = renderer4(Production)
  implicit val rendererRating: CsvRenderer[Rating] = renderer2(Rating.apply)
  implicit val rendererReviews: CsvRenderer[Reviews] = renderer7(Reviews)
  implicit val rendererName: CsvRenderer[Name] = renderer4(Name.apply)
  implicit val rendererPrincipal: CsvRenderer[Principal] = renderer2(Principal)
  implicit val rendererOptionPrincipal: CsvRenderer[Option[Principal]] = optionRenderer()
  val fAttributeSet: StringList => AttributeSet = AttributeSet.apply
  implicit val rendererAttributeSet: CsvRenderer[AttributeSet] = renderer1(fAttributeSet)

  /**
   * Renders a given Movie instance into a CSV-compatible string representation, with optional attributes provided to customize the rendering process.
   *
   * @param t     the Movie instance to be rendered.
   * @param attrs a map of attributes to customize the rendering of the Movie instance.
   *              These attributes can define how certain fields of the Movie should be processed or formatted.
   * @return a string representation of the Movie instance, formatted according to the provided attributes.
   */
  def render(t: Movie, attrs: Map[String, String]): String =
    renderer11(Movie.apply).render(t, attrs)
}
