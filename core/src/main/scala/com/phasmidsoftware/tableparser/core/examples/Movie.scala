/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.examples

import com.phasmidsoftware.tableparser.core.examples.MovieParser.camelToSnakeCaseColumnNameMapper
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Header, Table}
import scala.util.Try

/**
 * The `Movie` case class represents a comprehensive set of data about a movie, including its title, format, production details,
 * reviews, cast information, genres, plot keywords, and IMDb identifier. It encapsulates essential and optional attributes
 * about a movie in a structured format for easy parsing, manipulation, and serialization (e.g., CSV generation).
 *
 * @param title        The title of the movie, represented as a string.
 * @param format       The format details of the movie, as a `Format` object. This includes information such as color, language, aspect ratio, and duration.
 * @param production   The production details of the movie, as a `Production` object. This includes attributes such as country, budget, gross earnings, and title year.
 * @param reviews      The reviews and aggregated feedback data for the movie, as a `Reviews` object. It includes IMDb score, social metrics, and review counts.
 * @param director     The director of the movie, represented as a `Principal` object, which includes the director's name and Facebook likes.
 * @param actor1       The first main actor in the movie, represented as a `Principal` object.
 * @param actor2       The second main actor in the movie, represented as a `Principal` object.
 * @param actor3       The third main actor in the movie, represented as an optional `Principal` object. If no third actor is present, this will be `None`.
 * @param genres       The list of genres for the movie, represented as an `AttributeSet`. This may include genres like Action, Drama, Comedy, etc.
 * @param plotKeywords The set of plot-related keywords for the movie, represented as an `AttributeSet`. Keywords provide insights into the movie's storyline.
 * @param imdb         The unique IMDb identifier for the movie, represented as a string.
 */
case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Option[Principal], genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

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
object Movie extends CellParsers with CsvGenerators {
  val missing: Movie = apply("", Format.none, Production.none, Reviews.none, Principal.nemo, Principal.nemo, Principal.nemo, None, AttributeSet.none, AttributeSet.none, "")
  val header = "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes"
  implicit val helper: ColumnHelper[Movie] = columnHelper(camelToSnakeCaseColumnNameMapper,
    "title" -> "movie_title",
    "imdb" -> "movie_imdb_link")
  implicit val parser: CellParser[Movie] = cellParser11(apply)
  implicit val generator: CsvGenerator[Movie] = generator11(apply)
}

/**
 * Represents the format attributes associated with a movie, such as its color, language, aspect ratio, and duration.
 * This case class is immutable and provides a concise way to model format details.
 *
 * The `Format` case class includes the following fields:
 * - `color` (String): The color scheme of the movie (e.g., "Color", "Black and White").
 * - `language` (String): The language in which the movie is produced (e.g., "English", "French").
 * - `aspectRatio` (Option[Double]): An optional field representing the aspect ratio of the movie's visuals,
 * which is expressed as a decimal (e.g., `1.85`, `2.35`). If not provided, this field is `None`.
 * - `duration` (Option[Int]): An optional field representing the length of the movie in minutes. If not provided, this field is `None`.
 *
 * The `toString` method of the `Format` class provides a comma-separated string representation of its field values,
 * including `Option` fields, which are displayed as `None` when not defined.
 *
 * @param color       the color scheme of the movie's visuals.
 * @param language    the primary language of the movie.
 * @param aspectRatio an optional aspect ratio for the movie's visuals.
 * @param duration    an optional runtime of the movie in minutes.
 *
 *                    Usage example:
 * {{{
 * val format = Format("Color", "English", Some(1.85), Some(120))
 * println(format) // Outputs: Color,English,Some(1.85),Some(120)
 *
 * val defaultFormat = Format("", "", None, None)
 * println(defaultFormat) // Outputs: ,,,None,None
 * }}}
 */
case class Format(color: String, language: String, aspectRatio: Option[Double], duration: Option[Int]) {
  override def toString: String =
    s"$color,$language,$aspectRatio,$duration"
}

/**
 * The `Format` object provides a predefined constant for representing a default `Format` instance.
 * It serves as a utility object for commonly used instances of the `Format` case class.
 *
 * The default format instance (`none`) is initialized with empty or `None` values for fields,
 * representing a situation where no specific format details are available.
 *
 * @see Format The case class representing the format attributes such as color, language, aspect ratio, and duration.
 */
object Format extends CellParsers with CsvGenerators with CsvRenderers {
  val none: Format = Format("", "", None, None)
  implicit val helper: ColumnHelper[Format] = columnHelper(camelToSnakeCaseColumnNameMapper)
  implicit val parser: CellParser[Format] = cellParser4(apply)
  implicit val renderer: CsvRenderer[Format] = renderer4(apply)
  implicit val generatorOptInt: CsvGenerator[Option[Int]] = optionGenerator
  implicit val generatorOptDouble: CsvGenerator[Option[Double]] = optionGenerator
  implicit val generatorOptString: CsvGenerator[Option[String]] = optionGenerator
  implicit val generator: CsvGenerator[Format] = generator4(apply)
}

/**
 * The `Production` case class represents the metadata associated with a creative production,
 * such as a film, including its country of origin, budget, gross revenue, and release year.
 *
 * ==Fields==
 *
 * @param country   the country where the production originated, as a non-null `String`.
 * @param budget    an optional `Int` representing the production's budget. If unavailable, it is `None`.
 * @param gross     an optional `Int` representing the total gross revenue of the production. If unavailable, it is `None`.
 * @param titleYear an optional `Int` representing the year the production was released or titled. If unavailable, it is `None`.
 *
 *                  ==Methods==
 *                  - `isKiwi`: Determines if the production originates from New Zealand.
 *
 *                  ==Usage Example==
 * {{{
 * val production1 = Production("New Zealand", Some(1500000), Some(5000000), Some(2022))
 * val production2 = Production("USA", None, None, None)
 *
 * println(production1.isKiwi) // Output: true
 * println(production2.isKiwi) // Output: false
 * }}}
 */
case class Production(country: String, budget: Option[Int], gross: Option[Int], titleYear: Option[Int]) {
  def isKiwi: Boolean = this match {
    case Production("New Zealand", _, _, _) => true
    case _ => false
  }
}

/**
 * The `Production` object serves as a companion object for the `Production` case class.
 * It provides predefined constants and utility methods related to the concept of a production,
 * such as a film or other creative work.
 *
 * ==Overview==
 * - The `Production` object includes commonly used instances of the `Production` class.
 * - It facilitates operations on and interactions with `Production` objects in a consistent manner.
 *
 * @see [[Production]] case class for the associated data structure and detailed behavior.
 */
object Production extends CellParsers with CsvGenerators with CsvRenderers {
  val none: Production = Production("", None, None, None)
  implicit val helper: ColumnHelper[Production] = columnHelper(camelToSnakeCaseColumnNameMapper)
  implicit val parser: CellParser[Production] = cellParser4(apply)
  implicit val renderer: CsvRenderer[Production] = renderer4(apply)
  implicit val generator: CsvGenerator[Production] = generator4(apply)
}

/**
 * The `Reviews` case class represents aggregated review and user feedback data for a movie,
 * encapsulating information such as IMDb scores, social media metrics, content rating, and
 * review counts.
 *
 * @param imdbScore          The IMDb score of the movie, typically ranging from 1 to 10 with up to one decimal place.
 * @param facebookLikes      The number of likes the movie has on Facebook, used as a measure of its popularity on the platform.
 * @param contentRating      The `Rating` object indicating the content rating of the movie (e.g., PG, R), which may also include age restrictions.
 * @param numUsersReview     An `Option` representing the number of user reviews submitted for the movie. A value of `None` indicates an unknown or unavailable count.
 * @param numUsersVoted      The total number of users who voted for the movie on IMDb.
 * @param numCriticReviews   An `Option` representing the number of critic reviews submitted for the movie. A value of `None` indicates an unknown or unavailable count.
 * @param totalFacebookLikes The total combined Facebook likes of the movie's cast and crew, providing a broader measure of social media presence.
 */
case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Option[Int], numUsersVoted: Int, numCriticReviews: Option[Int], totalFacebookLikes: Int)

/**
 * The `Reviews` object contains utility members and predefined instances related to the `Reviews` case class.
 *
 * This object is primarily used to define default or placeholder instances of `Reviews`, such as the `none` value,
 * which represents a `Reviews` instance with default or empty data.
 */
object Reviews extends CellParsers with CsvGenerators with CsvRenderers {
  val none: Reviews = Reviews(0, 0, Rating(""), None, 0, None, 0)
  implicit val helper: ColumnHelper[Reviews] = columnHelper(camelToSnakeCaseColumnNameMapper,
    "facebookLikes" -> "movie_facebook_likes",
    "numUsersReview" -> "num_user_for_reviews",
    "numUsersVoted" -> "num_voted_users",
    "numCriticReviews" -> "num_critic_for_reviews",
    "totalFacebookLikes" -> "cast_total_facebook_likes")
  implicit val parser: CellParser[Reviews] = cellParser7(apply)
  implicit val renderer: CsvRenderer[Reviews] = renderer7(apply)
  implicit val generator: CsvGenerator[Reviews] = generator7(apply)
}
/**
 * Represents a principal entity involved in a production. This entity is characterized by its name
 * and the number of Facebook likes it has received. Typically, the `Principal` class is used to model
 * contributors such as actors, directors, or other individuals associated with a movie or production.
 *
 * @param name          the `Name` instance representing the principal's full name, which includes
 *                      fields for the first name, middle name (optional), last name, and suffix (optional).
 * @param facebookLikes the number of Facebook likes associated with the principal, which can serve
 *                      as a measure of the individual's popularity or reach on social media.
 * @example
 * {{{
 * val principal = Principal(Name("John", Some("A."), "Doe", None), 1200)
 * println(principal) // Output: John A. Doe (1200 likes)
 * }}}
 * @note The `Principal` class is part of a broader model for describing movies or productions,
 *       and may be used in conjunction with other classes such as `Movie` or `Production`.
 *       Additionally, a default instance of `Principal` may be provided in an accompanying
 *       companion object for known default values.
 */
case class Principal(name: Name, facebookLikes: Int) {
  override def toString = s"$name ($facebookLikes likes)"
}

/**
 * Companion object for the `Principal` case class, providing default values and utility objects
 * related to principal entities in a production context.
 *
 * This object currently includes a predefined default instance:
 *  - `nemo`: The default `Principal` instance with a default `Name` ("ne mo") and `facebookLikes` value of 0.
 *
 * ==Usage==
 * The `Principal` object, along with its case class, is typically used to represent individuals such as actors,
 * directors, or contributors in a production database system, with information about their name and popularity metrics.
 *
 * ==Predefined Instance==
 *  - `nemo`: A convenient default `Principal` instance, used as a placeholder or fallback.
 *
 * @example Using the default `Principal`:
 * {{{
 * println(Principal.nemo) // Output: ne mo (0 likes)
 * }}}
 *
 *          ==Relation==
 *          Works in conjunction with the `Principal` case class and is associated with other models in parsing, processing,
 *          and rendering movie-related data structures.
 */
object Principal extends CellParsers with CsvGenerators with CsvRenderers {
  val nemo: Principal = Principal(Name.nemo, 0)
  implicit val helper: ColumnHelper[Principal] = columnHelper(camelToSnakeCaseColumnNameMapper, Some("$x_$c"))
  implicit val parser: CellParser[Principal] = cellParser2(apply)
  implicit val parserOpt: CellParser[Option[Principal]] = cellParserOption
  implicit val renderer: CsvRenderer[Principal] = renderer2(apply)
  implicit val rendererOpt: CsvRenderer[Option[Principal]] = optionRenderer()
  implicit val generator: CsvGenerator[Principal] = generator2(apply)
  implicit val generatorOpt: CsvGenerator[Option[Principal]] = optionGenerator
}

/**
 * A case class representing a person's name with optional middle name and suffix.
 *
 * @param first  The first name of the person (mandatory).
 * @param middle An optional middle name of the person.
 * @param last   The last name (or family name) of the person (mandatory).
 * @param suffix An optional suffix, such as "Jr.", "Sr.", or other titles.
 *
 *               The `Name` class overrides the `toString` method to provide a concatenated
 *               string representation of the full name. The full name includes the first name,
 *               optionally followed by the middle name, then the last name, and finally an
 *               optional suffix. Components are separated by a space.
 *
 *               Example usage:
 * {{{
 * val fullName = Name("John", Some("William"), "Doe", Some("Jr."))
 * println(fullName.toString) // Output: "John William Doe Jr."
 *
 * val simpleName = Name("Jane", None, "Smith", None)
 * println(simpleName.toString) // Output: "Jane Smith"
 * }}}
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
object Name extends CellParsers with CsvGenerators with CsvRenderers {
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

  val nemo: Name = Name("ne mo")
  implicit val parser: CellParser[Name] = cellParser(apply)
  implicit val renderer: CsvRenderer[Name] = renderer4(apply)
  implicit val generator: CsvGenerator[Name] = generator4(apply)
}

/**
 * Represents a content rating, typically used to signify the suitability of media content
 * (e.g., movies, shows) for different audiences. The `Rating` class consists of a code
 * (e.g., "PG", "R", "G") and an optional age specification (e.g., "PG-13").
 *
 * @param code The code representing the general content rating (e.g., "PG", "R").
 * @param age  An optional integer specifying the age restriction for the content, if applicable
 *             (e.g., a value of `13` for "PG-13").
 */
case class Rating(code: String, age: Option[Int]) {
  override def toString: String = code + (age match {
    case Some(x) => "-" + x
    case _ => ""
  })
}

/**
 * Companion object for the `Rating` case class.
 * This object provides utility methods to construct `Rating` instances and to parse rating strings into `Rating`.
 */
object Rating extends CellParsers with CsvGenerators with CsvRenderers {
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

  implicit val parser: CellParser[Rating] = cellParser(apply)
  implicit val renderer: CsvRenderer[Rating] = renderer2(apply)
  implicit val generator: CsvGenerator[Rating] = generator2(apply)
  private val rRating = """^(\w*)(-(\d\d))?$""".r
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
