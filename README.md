[![Codacy Badge](https://api.codacy.com/project/badge/Grade/1dc65c7cf84e46bfbb0d3d9b16c0f382)](https://app.codacy.com/app/scalaprof/TableParser?utm_source=github.com&utm_medium=referral&utm_content=rchillyard/TableParser&utm_campaign=Badge_Grade_Settings)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.phasmidsoftware/tableparser_2.13/badge.svg?color=blue)](https://maven-badges.herokuapp.com/maven-central/com.phasmidsoftware_2.13/tableparser/)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/TableParser)
![GitHub](https://img.shields.io/github/license/rchillyard/TableParser)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/TableParser)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/TableParser)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/TableParser/bug)

# Introduction to TableParser

A functional parser of tables implemented in Scala.
Typically, the input is in the form of a "CSV" (comma-separated-values) file.
However, it is perfectly possible to parse other formats.

_TableParser_ aims to make it as simple as possible to ingest a strictly-typed tabular dataset.
The principal mechanism for this is the use of case classes to specify the types of fields in the dataset.
All conversions from strings to standard types are performed automatically.
For non-standard types, it suffices simply to provide an implicit converter of the form _String=>T_.

It is possible to parse sequences of _String_ (one per row)--the typical situation for a CSV file--or sequences of sequences of _String_
(where the table corresponds to a matrix of cells).
 
This library makes extensive use of type classes and other implicit mechanisms.
Indeed, it is implemented very similarly to JSON readers.
There is a row-parser configuration mechanism which allows
the programmer to vary the regular expressions for recognizing
strings and delimiters, also to vary the quote character.

In addition to parsing, _TableParser_ provides a mechanism for rendering a table in _hierarchical_ form (for example for
XML or HTML).
An output structure which is itself tabular or sequence-oriented can be generated quite easily using the rows of the table,
together with something like, for instance, a Json writer.

Quick Intro
===========

The simplest way to get an introduction to _TableParser_ is to consult the airbnb.sc and movie.sc worksheets.
These give detailed descriptions of each stage of the process.

Take a look also at the _Main_ object in the  _Crime.scala_ module (it's under the _test_ directory).
The model is relatively simple, but not too simple.
There are 12 columns in total, but five have been grouped into _CrimeLocation_, with the remaining seven at the top level, i.e., in _Crime_.
The members _CrimeID_ and _CrimeLocation_ are optional.
A sample data file is located in com/phasmidsoftware/examples/crime/2023-01-metropolitan-street-sample.csv.
The full file can be downloaded from Kaggle (see code) in _Crime_.

One possibility is to run an analysis on the data (see CrimeSpec: Crime/be ingested and analyzed as a RawTable).
In order to read the dataset as a _Table\[Crime]_.

    import CrimeParser._
    val cti: IO[Table[Crime]] = Table.parseResource(Crime.sampleFile, classOf[Crime])
    matchIO(cti, Timeout(Span(60, Seconds))) {
      case table@HeadedTable(_, _) =>
        // operate on table
    }

Notice that the parsed table is wrapped inside _IO_, the Cats I/O monad.
This has some technical advantages over using _Future_ or _Try_, which we won't detail here.

Another way to see how it works is to look at this application _Pairings_ which takes a CSV file, parses it, transforms the data,
and outputs a JSON file.
This way of parsing is a little different from what is shown in the worksheets.
But both are effective.
The minimum code necessary to read parse the CSV file as a table of "Player"s, using as many defaults as possible is:

    case class Player(first: String, last: String)

    object Player extends TableParserHelper[Player]() {
      def cellParser: CellParser[Player] = cellParser2(apply)
    }

    val pty: IO[Table[Player]] = Table.parseFile[Table[Player]]("players.csv")

This assumes that the source input file ("players.csv") contains a header row which includes column names corresponding to the parameters
of the case class _Player_ (in this case "first" and "last").
If, for example, your CSV file does not have a header row, then you make a minor change to the line _object Player..._

The input file looks something like this (the first and last columns are required, others are ignored):

    Id,First,Last,
    1,Adam,Sullivan,
    2,Amy,Avagadro,
    3,Annaa,Peterson,

etc...

Note that columns not needed for the _Player_ case class are simply ignored.
Also, note that the case of the column names is not important.

For another simple use case _TableParser_, please see my blog at: https://scalaprof.blogspot.com/2019/04/new-projects.html 

# User Guide

Current version: 1.1.3.

See release notes below for history.

Parsing
=======

The _Table_ trait expresses the result of parsing from a representation of a table.
Each row is represented by a parametric type _Row_.
Typically, this _Row_ type is a case class with one parameter corresponding to one column in the table file.
However, some table files will have too many columns to be practical for this correspondence.
In such a situation, you have two choices:
(1) parsing each row as a list of String (also known as a "raw" row);
(2) parsing each row as a hierarchical arrangement of case classes (or tuples).
Typically, especially if the dataset is new to you,
you will start with (1) and run an analysis on the columns to help you design the classes for option (2).

For the first option, you will do something like the following (see the _AnalysisSpec_ unit tests):

    private val sampleFile = "2023-01-metropolitan-street-sample.csv"
    private val triedSampleResource: Try[URL] = FP.resource[Analysis](sampleFile)
    val fraction = 4
    val parser = RawTableParser().setPredicate(TableParser.sampler(fraction))
    val ui = IOUsing(for (u <- triedSampleResource) yield Source.fromURL(u)) {
        s => parser.doParse(s) map (rawTable => println(Analysis(rawTable)))
    }
    ui.unsafeRunSync()

This analysis will give you a list of columns, each showing its name, size, and
whether it is optional (i.e. contains nulls), together with an _Analytic_:
* if it's a numerical column: its range, mean, and standard deviation.
* if it's a column made up of a relatively small number of classes:
a histogram giving the class names with frequency, in order of decreasing frequency.

Note the use of the predicate and sampler.
This allows you to randomly choose a subset of the rows.
In the example given, approximately one quarter of the rows will be chosen.

Incidentally, this raw parser has three signatures, one for resources, one for files, and one for a sequence of Strings.
And the default for raw row parsing is to allow quoted strings to span multiple lines.

But, if not parsing as raw rows, you will need to design a class hierarchy to model the columns of the table.
_TableParser_ will take care of any depth of case classes/tuples.
Currently, there is a limit of 12 parameters per case class/tuple so with a depth of _h_ classes/tuples you could
handle _12^h_ attributes altogether.

The names of the parameters of a case class do not necessarily have to be the same as the column from which the value derives.
The _ColumnHelper_ class is available to manage the mapping between parameters and columns.

The result of parsing a table file (CSV, etc.) will be a _Table\[Row]_, wrapped in _Try_.
There are object methods to parse most forms of text: _File, Resource, InputStream, URL, Seq\[String]_, etc. (see _Table_ below).

The parser responsible for parsing the contents of a cell is called _CellParser\[T]_ where T is the type of the value in the cell in question.
T is covariant so that if you have alternative parsers which generate different subclasses of trait, for instance, this can be done.

In order for _TableParser_ to know how to construct a case class (or tuple) from a set of values,
an implicit instance of _CellParser\[T]_ must be in scope.
This is achieved via invoking a method (from object _Parsers_) of the following form:
where _f_ is a function which takes _N_ parameters of types _P1, P2, ... Pn_ respectively,
and where _T_ is the type to be constructed:

    cellParserN[T,P1,P2,...Pn](f)
 
Typically, the function _f_ is the _apply_ method of the case class _T_,
although you may have to explicitly refer to a particular function/method with a specific signature.
When you have created a companion object to the case class, you will simply use the method name (typically _apply_) as in
_Name.apply_ (see example below).
If you have created additional apply methods, you will need to define a function of a specific type and pass that in.
Or, more simply, do as for _ratingParser_ in the example below.

Note that _P1_, _P2_, ... _Pn_ each have a context bound on _CellParser_ (that's to say, there is implicit
evidence of type _CellParser\[P]_).
This is the mechanism which saves the programmer from having to specify explicit conversions.
_T_ is bound to be a subtype of _Product_ and has two context bounds: _ClassTag_ and _ColumnHelper_.

See section on _CellParsers_ below.

## Table

The _Table_ class, which extends _Iterable\[Row]_, also has several methods for manipulation:
### query methods
* def content: Content\[Row]
* def maybeHeader: Option\[Header]
* def toCSV(implicit renderer: CsvRenderer\[Row], generator: CsvProductGenerator\[Row], csvAttributes: CsvAttributes): Iterable\[String]
* def maybeColumnNames: Option\[Seq\[String]]
* def column(name: String): Iterator\[Option\[String]]
* writeCSVFile(file: File)(implicit renderer: CsvRenderer\[Row], generator: CsvGenerator\[Row], ordering: Ordering\[Row], csvAttributes: CsvAttributes): Unit
* def writeCSVFileEncrypted[A: HexEncryption](file: File)(implicit renderer: CsvRenderer\[Row], generator: CsvGenerator\[Row], ordering: Ordering\[Row], hasKey: HasKey\[Row], csvAttributes: CsvAttributes): Unit

### transformation methods
* def filter(p: Row => Boolean): Table\[Row]
* def filterNot(p: Row => Boolean): Table\[Row]
* def filterValid(implicit rv: Validity\[Row]): Table\[Row]
* def map\[S](f: Row => S): Table\[S]
* def flatMap\[U](f: Row => Iterable\[U]): Table\[U]
* def mapOptional\[S](f: Row => Option\[S]): Table\[S]
* def unit\[S](sc: Content\[S], maybeHeader: Option\[Header]): Table\[S]
* def unit\[S](rows: Iterable\[S], maybeHeader: Option\[Header]): Table\[S]
* def ++\[U >: Row](table: Table\[U]): Table\[U]
* def zip\[R](rt: Table\[R]): Table\[(Row, R)]
* def processRows\[S](f: Iterable\[Row] => Iterable\[S]): Table\[S]
* def processRows\[R, S](f: (Iterable\[Row], Iterable\[R]) => Iterable\[S])(other: Table\[R]): Table\[S]
* def sort\[S >: Row : Ordering]: Table\[S]
* def select(range: Range): Table\[Row]
* def select(n: Int): Table\[Row]
* def drop(n: Int): Table\[Row]
* def dropWhile(p: Row => Boolean): Table\[Row]
* def take(n: Int): Table\[Row]
* def takeWhile(p: Row => Boolean): Table\[Row]
* def sample(n: Int)(implicit random: Random): Table\[Row]
* def slice(from: Int, until: Int): Table\[Row]
* lazy val shuffle: Table\[Row]

It is to be expected that _join_ methods will be added later (based upon the second signature of processRows).

The following **object** methods are available for parsing text in _Table_:
* def parse\[T: TableParser](ws: Iterable\[String]): IO\[T]
* def parse\[T: TableParser](ws: Iterator\[String]): IO\[T]
* def parseSource\[T: TableParser](x: => Source): IO\[T]
* def parse\[T: TableParser](si: => IO\[Source]): IO\[T]
* def parse\[T: TableParser](u: URI)(implicit codec: Codec): IO\[T]
* def parse\[T: TableParser](u: URI, enc: String): IO\[T]
* def parseInputStream\[T: TableParser](i: InputStream)(implicit codec: Codec): IO\[T]
* def parseInputStream\[T: TableParser](i: InputStream, enc: String): IO\[T]
* def parseFile\[T: TableParser](f: File)(implicit codec: Codec): IO\[T]
* def parseFile\[T: TableParser](f: File, enc: String): IO\[T]
* def parseFile\[T: TableParser](pathname: String)(implicit codec: Codec): IO\[T]
* def parseFile\[T: TableParser](pathname: String, enc: String): IO\[T]
* def parseResource\[T: TableParser](s: String, clazz: Class\[_] = getClass)(implicit codec: Codec): IO\[T]
* def parseResource\[T: TableParser](u: URL, enc: String): IO\[T]
* def parseResource\[T: TableParser](u: URL)(implicit codec: Codec): IO\[T]
* def parseSequence\[T: TableParser](wss: Seq\[Seq\[String]]): IO\[T]
* def parseFileRaw(f: File, predicate: Try\[RawRow] => Boolean, maybeFixedHeader: Option\[Header] = None, forgiving: Boolean = true)(implicit codec: Codec): IO\[Table\[RawRow]]
* def parseFileRaw(pathname: String, predicate: Try\[RawRow] => Boolean)(implicit codec: Codec): IO\[Table\[RawRow]]

Please note that, in the case of a parameter being an Auto-closeable object such as _InputStream_ or Source,
it is the caller's responsibility to close it after parsing.
However, if the parameter is a File, or filename, or URL/URI, then any Source object that is instantiated within
the parse method will be closed.
This applies also to the _parseInputStream_ methods: the internally defined _Source_ will be closed (but not the stream).

Additionally, there is an implicit class called _ImplicitParser_ (defined in the _TableParser_ companion object)
which allows for expressions such as:

    parser parse source

This is the recommended way to parse because it is the simplest.
It also allows chaining of "lens" methods to configure the parser, for example:

    val parser = RawTableParser().setPredicate(TableParser.sampler(2)).setMultiline(true)

## TableParser

_TableParser_ is also the name of a trait which takes a parametric type called "Table" in its definition.
It is defined thus:

    trait TableParser[Table] {
      type Row
      def hasHeader: Boolean
      def forgiving: Boolean = false
      def multiline: Boolean = false
      val predicate: Try[Row] => Boolean = includeAll
      def rowParser: RowParser[Row]
      def builder(rows: Seq[Row]): Table
      def parse(ws: Seq[String]): IO[Table] = ...
}

The type _Row_ defines the specific row type (for example, _Movie_, in the example below).
_hasHeader_ is used to define if there is a header row in the first line of the file (or sequence of strings) to be parsed.
_forgiving_, which defaults to _false_, can be set to _true_ if you expect that some rows will not parse, but where this
will not invalidate your dataset as a whole.
_multiline_ is used to allow (or disallow when false) quoted strings to span multiple lines.

In forgiving mode, any exceptions thrown in the parsing of a row are collected and then logged.
_rowParser_ is the specific parser for the _Row_ type (see below).
_builder_ is used by the _parse_ method.
_parse_ is the main method of _TableParser_ and takes a _Seq\[String]_ and yields a _Try\[Table]_.

The predicate is used to filter rows (which are the results of parsing).
By default, all rows are included.
_TableParser_ also provides a method (_sampler_) to create a random sampling function.
Note, however, that a significant part of the time for building a table from a large file is just reading and parsing the file.
Sampling will not reduce this portion of the time.

Associated with _TableParser_ is an abstract class called _TableParserHelper_ whose purpose is to make your coding job easier.
_TableParserHelper_ is designed to be extended (i.e. sub-classed) by the companion object of the case class that you
wish to parse from a row of your input.
Doing it this way makes it easier for the implicit TableParser instance to be found.
You can also set up your application along the lines of the examples below, such as the Movie example.

The constructor for _TableParserHelper_ takes two parameters, both of which can be defaulted:
* sourceHasHeaderRow: Boolean = true
* forgiving: Boolean = false

## RowParser

_RowParser_ is a trait which defines how a line of text is to be parsed as a _Row_.
_Row_ is a parametric type which, in subtypes of _RowParser_, is context-bound to _CellParser_.
A second parametric type _Input_ is defined: this will take on values of _String_ or _Seq\[String]_, according to the form of input.
Typically, the _StandardRowParser_ is used, which takes as its constructor parameter a _LineParser_.

The methods of _RowParser_ are:

    def parse(w: String)(header: Header): IO[Row]

    def parseHeader(w: String): IO[Header]

## LineParser

The _LineParser_ takes five parameters: two regexes, a String and two Chars.
These define, respectively, the delimiter regex, the string regex, list enclosures, the list separator, and the quote character.
Rather than invoke the constructor directly, it is easier to invoke the companion object's _apply_ method, which takes a single implicit parameter: a _RowConfig_.
Two consecutive quote characters, within a quoted string, will be parsed as a single quote character.
The _LineParser_ constructor will perform some basic checks that its parameters are consistent.

## StringsParser

_StringsParser_ is a trait which defines an alternative mechanism for converting a line of text to a _Row_.
As with the _RowParser_, _Row_ is a parametric type which is context-bound to _CellParser_.
_StringsParser_ is useful when the individual columns have already been split into elements of a sequence.
Typically, the _StandardStringsParser_ is used.

The methods of _StringsParser_ are:

    def parse(ws: Seq[String])(header: Header): IO[Row]

    def parseHeader(ws: Seq[String]): IO[Header]

## CellParsers

There are a number of methods which return an instance of _CellParser_ for various situations:

* def cellParserRepetition\[P: CellParser : ColumnHelper](start: Int = 1): CellParser\[Seq\[P]]
* def cellParserSeq\[P: CellParser]: CellParser\[Seq\[P]]
* def cellParserOption\[P: CellParser]: CellParser\[Option\[P]]
* def cellParserOptionNonEmptyString: CellParser\[Option\[String]]
* def cellParser\[P: CellParser, T: ClassTag](construct: P => T): CellParser\[T]
* def cellParser1\[P1: CellParser, T <: Product : ClassTag : ColumnHelper](construct: P1 => T, fields: Seq\[String] = Nil): CellParser\[T]
* etc. through cellParser12...
* def cellParser2Conditional\[K: CellParser, P, T <: Product : ClassTag : ColumnHelper](construct: (K, P) => T, parsers: Map\[K, CellParser\[P]], fields: Seq\[String] = Nil): CellParser\[T]

The methods of form _cellParserN_ are the parsers which are used to parse into case classes.
Ensure that you have the correct number for N: the number of fields/parameters in the case class you are instantiating.
In some situations, the reflection code is unable to get the field names in order (for example when there are public
lazy values).
In such a case, add the second parameter to _explicitly_ give the field names in order.
Normally, of course, you can leave this parameter unset.

There is one additional method to handle the situation where you want to vary the parser for a set of cells according
to the value in another (key) column: _cellParser2Conditional_.
In this case, you must supply a _Map_ which specifies which parser is to be used for each possible value of the key column.
If the value in that column is not one of the keys of the map, an exception will be thrown.
For an example of this, please see the example in _CellParsersSpec_ ("conditionally parse").

## Content

The rows of a _Table_ are represented by a case class called _Content_:

    case class Content[+Row](private val xs: ParIterable[Row])

Currently, the internal rows are represented by a _ParIterable\[Row]_ which holds the rows in parallel partitions.
This necessarily shuffles the ordering of the rows.

### Sequence and Sequential

Tables can be ordered explicitly or they can be ordered by a _Sequence_ member whose values are generated by the parser.
The trait _Sequential_ enables the definition of type constructors which can provide evidence of the 
corresponding order.

For an example of this in use, see the _Crime_ class:

    case class Crime(sequence: Sequence,
        maybeCrimeId: Option[BigInt],
        month: String,
        reportedBy: String,
        fallsWithin: String,
        maybeLocation: Option[CrimeLocation],
        crimeType: String,
        lastOutcomeCategory: String,
        context: String) extends Sequential

There is no column in the CSV corresponding to _sequence_.
However, the parser auto-generates that column.

We can parse the file and write out a one-tenth sample with something like the following:

    import CrimeParser._
    import cats.effect.unsafe.implicits.global
    implicit val random: Random = new Random(0)
    val sampleFile = "2023-01-metropolitan-street-sample.csv"
    val outputFile = "tmp/Crime.use.Resource.csv"
    val writeResource = Resource.make(IO(new FileWriter(outputFile)))(fw => IO(fw.close()))
    val wi: IO[Unit] = for {
      url <- ioResource[Crime](sampleFile)
      readResource = Resource.make(IO(Source.fromURL(url)))(src => IO(src.close()))
      ct <- readResource.use(src => Table.parseSource(src))
      lt <- IO(ct.mapOptional(m => m.brief))
      st <- IO(lt.filter(FP.sampler(10)))
      w <- st.toCSV
      _ <- writeResource.use(fw => IO(fw.write(w)))
    } yield ()
    wi.unsafeRunSync()

## Caveats

A case class which represents a row (or part of a row) of the table you want to create from parsing,
or which you want to render must abide by certain rules:
* There should not be any fields defined in the body of the case class.
So, no <i>val, var</i> or <i>lazy val</i>.
Instead, any behavior you want to add to the class, beyond the parameters (fields) of the class,
must be defined using _def_.

## Example: Movie

In this example, we parse the IMDB Movie dataset from Kaggle.
The basic structure of the application code will look something like this:

        import MovieParser._
        val x: IO[Table[Movie]] = Table.parseResource("movie_metadata.csv")
     
In this example, the row type is _Movie_, a case class with eleven parameters.
The data can be found in a local resource (relative to this class) called movie_metadata.csv.
All the (implicit) details that characterize this particular table input are provided
in the _MovieParser_ object.

The _Movie_ class looks like this:

    case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Option[Principal], genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

Note that we make _actor3_ optional because some movies don't specify an "actor3".
Unlike with ordinary values such as _Int_, _Double_, we do have to add an additional
_implicit_ definition to accomplish this (see in example code below):
 
    implicit val optionalPrincipalParser: CellParser[Option[Principal]] = cellParserOption
 
The other case classes look like this:

    case class Format(color: String, language: String, aspectRatio: Double, duration: Int)
    case class Production(country: String, budget: Option[Int], gross: Int, titleYear: Int)
    case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Int, numUsersVoted: Int, numCriticReviews: Int, totalFacebookLikes: Int)
    case class Principal(name: Name, facebookLikes: Int)
    case class Name(first: String, middle: Option[String], last: String, suffix: Option[String])
    case class Rating(code: String, age: Option[Int])

The _MovieParser_ object looks like this:

    object MovieParser extends CellParsers {
        def camelToSnakeCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z0-9])", "_$1")
        implicit val movieColumnHelper: ColumnHelper[Movie] = columnHelper(camelToSnakeCaseColumnNameMapper _,
            "title" -> "movie_title",
            "imdb" -> "movie_imdb_link")
        implicit val reviewsColumnHelper: ColumnHelper[Reviews] = columnHelper(camelToSnakeCaseColumnNameMapper _,
            "facebookLikes" -> "movie_facebook_likes",
            "numUsersReview" -> "num_user_for_reviews",
            "numUsersVoted" -> "num_voted_users",
            "numCriticReviews" -> "num_critic_for_reviews",
            "totalFacebookLikes" -> "cast_total_facebook_likes")
        implicit val formatColumnHelper: ColumnHelper[Format] = columnHelper(camelToSnakeCaseColumnNameMapper _)
        implicit val productionColumnHelper: ColumnHelper[Production] = columnHelper(camelToSnakeCaseColumnNameMapper _)
        implicit val principalColumnHelper: ColumnHelper[Principal] = columnHelper(camelToSnakeCaseColumnNameMapper _, Some("$x_$c"))
        implicit val ratingParser: CellParser[Rating] = cellParser(Rating.apply: String => Rating)
        implicit val formatParser: CellParser[Format] = cellParser4(Format)
        implicit val productionParser: CellParser[Production] = cellParser4(Production)
        implicit val nameParser: CellParser[Name] = cellParser(Name.apply)
        implicit val principalParser: CellParser[Principal] = cellParser2(Principal)
        implicit val reviewsParser: CellParser[Reviews] = cellParser7(Reviews)
        implicit val attributesParser: CellParser[AttributeSet] = cellParser(AttributeSet.apply: String => AttributeSet)
        implicit val optionalPrincipalParser: CellParser[Option[Principal]] = cellParserOption
        implicit val movieParser: CellParser[Movie] = cellParser11(Movie)
        implicit object MovieConfig extends DefaultRowConfig {
            override val string: Regex = """[^,]*""".r
            override val delimiter: Regex = """,""".r
            override val listEnclosure: String = ""
        }
        implicit val parser: StandardRowParser[Movie] = StandardRowParser[Movie]
        implicit object MovieTableParser extends StringTableParser[Table[Movie]] {
            type Row = Movie
            val maybeFixedHeader: Option[Header] = None
            override val forgiving: Boolean = true
            val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
            protected def builder(rows: Iterator[Movie], header: Header): Table[Row] = HeadedTable(rows, header)
        }
    }

In this code,
_movieColumnHelper_, and the other columnHelpers, specify parameter-column mappings.

Note that _principalColumnHelper_ has an extra parameter at the start of the parameter list:
    
    Some("$x_$c")
    
which is an (optional) formatter for the purpose of prefixing a string to column names.
That's because there are several "Principal" parameters in a _Movie_, and each one has its own set of attributes. 
In this format parameter, "$x" is substituted by the prefix (the optional value passed into the lookup method)
while $c represents the translated column name.

Even when there is no name translation necessary, we still have to provide a columnHelper, as in:

    implicit val ratingColumnHelper: ColumnHelper[Rating] = columnHelper()

A couple of parameters of _Movie_ are actually attribute sets (_AttributeSet_).
These are basically lists of String within one column value.
Such lists are parsed as lists as they are parsed from the original strings and then returned as strings
in the form "{" element "," element ... "}"
The parsing from the original string obeys the _RowConfig_ parameters of _listSep_ and _listEnclosure_.

In this case, the row config object is defined as _MovieConfig_.
There, you also see the parameters _string_ which is a regular expression for a string in a table cell;
and _delimiter_ which is a regular expression which defines the separator between table columns;
and _quote_ the quote character which can be used to include cell values which enclose the separator.

A parameter can be optional, for example, in the _Movie_ example, the _Production_ class is defined thus:

    case class Production(country: String, budget: Option[Int], gross: Int, title_year: Int)
    
In this example, some movies do not have a budget provided.
All you have to do is declare it optional in the case class and _TableParser_ will specify it as _Some(x)_ if valid, else _None_.

Note that there is a default, implicit _RowConfig_ object defined in the object _RowConfig_.

## Example: Submissions

This example has two variations on the earlier theme of the _Movies_ example:
(1) each row (a Submission) has an unknown number of _Question_ parameters;
(2) instead of reading each row from a single String, we read each row from a sequence of Strings, each corresponding to a cell.

The example comes from a report on the submissions to a Scala exam. Only one question is included in this example.

      case class Question(question_ID: String, question: String, answer: Option[String], possible_points: Int, auto_score: Option[Double], manual_score: Option[Double])
      case class Submission(username: String, last_name: String, first_name: String, questions: Seq[Question])
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
            val maybeFixedHeader: Option[Header] = None
            protected def builder(rows: Iterator[Row], header: Header): Table[Row] = HeadedTable(rows, header)
            override val forgiving: Boolean = false
            val rowParser: RowParser[Row, Seq[String]] = implicitly[RowParser[Row, Seq[String]]]
        }
      val rows: Seq[Seq[String]] = Seq(
          Seq("Username", "Last Name", "First Name", "Question ID 1", "Question 1", "Answer 1", "Possible Points 1", "Auto Score 1", "Manual Score 1"),
          Seq("001234567s", "Mr.", "Nobody", "Question ID 1", "The following are all good reasons to learn Scala -- except for one.", "Scala is the only functional language available on the Java Virtual Machine", "4", "4", "")
        )

      import Submissions._
      val qty: IO[Table[Submission]] = Table.parseSequence(rows)

Note the use of _cellParserRepetition_. The parameter allows the programmer to define the start value of the sequence number for the columns.
In this case, we use the default value: 1 and so don't have to explicitly specify it.
Also, note that the instance of _ColumnHelper_ defined here has the formatter defined as "$c $x" which is in the opposite order from the Movie example.

Rendering
=========

_TableParser_ provides a general mechanism for rendering (serializing to text) tables.
Indeed, _Table\[Row]_ extends _Renderable\[Row]_ which supports the _render(implicit rs: StringRenderer\[Row])_ method. 
two mechanisms for rendering a table:
* one to a straight serialized output, for example, when rendering a table as a CSV file.
* the other to a hierarchical (i.e. tree-structured) output, such as an HTML file.

## Non-hierarchical output

For this type of output, the application programmer must provide an instance of _Writer\[O]_ which is, for example a _StringBuilder_,
_BufferedOutput_, or perhaps an I/O Monad.

The non-hierarchical output does not support the same customization of renderings as does the hierarchical output.
It's intended more as a straight, quick-and-dirty output mechanism to a CSV file.

Here, for example, is an appropriate definition.

	implicit object StringBuilderWriteable extends Writable[StringBuilder] {
		def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
		def unit: StringBuilder = new StringBuilder
		override def delimiter: CharSequence = "|"
	}
	
The default _delimiter_ is ", ".
You can override the _newline_ and _quote_ methods too if you don't want the defaults.	

And then, following this, you will write something like the following code:

    print(table.render.toString)

The _Writable_ object will take care of inserting the delimiter and quotes as appropriate.
Columns will appear in the same order as the parameters of _Row_ type (which must be either a _Product_, such as a case class, or an _Array_ or a _Seq_).
If you need to change the order of the rows, you will need to override the _writeRow_ method of _Writable_.
 
## Hierarchical rendering

A type class called _TreeWriter_ is the main type for hierarchical rendering.
One of the instance methods of _Table\[Row]_ is a method as follows:

    def renderHierarchical\[U: TreeWriter](style: String)(implicit rr: HierarchicalRenderer[Row]): U
    
Providing that you have defined an implicit object of type _TreeWriter\[U]_ and a _HierarchicalRenderer\[Row]_,
then the _renderHierarchical_ method will produce an instance of _U_ which will be a tree containing all the rows of this table.

What sort of type is _U_?
An XML node would be appropriate.
The specifications use a type called HTML which is provided in package _parse.render.tag_ more as an exemplar rather than something definitive.

    case class HTML(tag: String, content: Option[String], attributes: Map[String, String], hs: Seq[HTML])

The example _TreeWriter_ for this type is reproduced here:

    trait TreeWriterHTML$ extends TreeWriter[HTML] {
    	def addChild(parent: HTML, child: HTML): HTML = parent match {
    		case HTML(t, co, as, hs) => HTML(t, co, as, hs :+ child)
    	}
    	def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[HTML]): HTML = HTML(tag, content, attributes, children)
    	
    implicit object TreeWriterHTML$ extends TreeWriterHTML$

If we have a row type as for example:

	case class Complex(r: Double, i: Double)
	
Then, we should define appropriate renderers along the following likes:

	implicit val valueRenderer: HierarchicalRenderer[Double] = renderer("td")
	implicit val complexRenderer: HierarchicalRenderer[Complex] = renderer2("tr")(Complex)

We can then write something like:

	val table = HeadedTable(Seq(Complex(0, 1), Complex(-1, 0)), Header.create("r", "i"))
	val h = table.renderHierarchical("table", Map("border" -> "1"))
	 
The result of this will be an HTML tree which can be written out thus as a string:
	 
	 <table border="1">
     <tr>
     <td name="r">0.0</td>
     <td name="i">1.0</td></tr>
     <tr>
     <td name="r">-1.0</td>
     <td name="i">0.0</td></tr></table>

As with the parsing methods, the conversion between instances of types (especially case classes) and Strings is hierarchical (recursive).

If you need to set HTML attributes for a specific type, for example a row in the above example, then an attribute map can be defined for the _renderer2_ method.

## CSV Rendering

If you simply need to write a table to CSV (comma-separated value) format as a _String_, then use the _toCsv_ method of _Table\[T]_.
Note that there is also an object method of _Table_ called _toCsvRow_ which can be used for instances of _Table\[Row]_.
More control can be gained by using _CsvTableStringRenderer\[T]_ or _CsvTableFileRenderer\[T]_ for a particular type _T_.

These require customizable (implicit) evidence parameters and are defined as follows:

    case class CsvTableStringRenderer[T: CsvRenderer : CsvGenerator]()(implicit csvAttributes: CsvAttributes)
        extends CsvTableRenderer[T, StringBuilder]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.stringBuilderWritable(csvAttributes.delimiter, csvAttributes.quote), csvAttributes)
    case class CsvTableFileRenderer[T: CsvRenderer : CsvGenerator](file: File)(implicit csvAttributes: CsvAttributes)
        extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.fileWritable(file), csvAttributes)
    abstract class CsvTableRenderer[T: CsvRenderer : CsvGenerator, O: Writable]()(implicit csvAttributes: CsvAttributes) extends Renderer[Table[T], O] {...}

_CsvRenderer\[T]_ determines the layout of the rows, while _CsvGenerator\[T]_ determines the header.
_CsvAttributes_ specify the delimiter and quote characters for the output.
Instances of each can be created using methods in _CsvRenderers_ and _CsvGenerators_ respectively.
Appropriate methods are:
* sequenceRenderer, optionRenderer, renderer1,  renderer2, renderer3, etc. up to renderer12.
* sequenceGenerator, optionGenerator, generator1,  generator2, generator3, etc. up to generator12.

In some situations, you will want to omit values (and corresponding header columns) when outputting a CSV file.
You may use the following methods (from the same types as above):

    def skipRenderer[T](alignment: Int = 1)(implicit ca: CsvAttributes): CsvRenderer[T] 
    def skipGenerator[T](implicit ca: CsvAttributes): CsvGenerator[T]
    
Note that, when rendering a CSV row, you may want to simply render some number of delimiters
(this would be in the case where you have a fixed header).
You can use the _alignment_ parameter of _skipRenderer_ to ensure alignment is correct.

As usual, the standard types are pre-defined for both _CsvRenderer\[T]_ and _CsvGenerator\[T]_ (for Int, Double, etc.).

The methods mentioned above render tables in the form of CSV Strings.
However, there are also methods available to render tables as a _File_: _writeCSVFile_ and _writeCSVFileRow_.
These utilize the type _CsvTableFileRenderer\[T]_ mentioned above.

If you wish to output only a subset of rows, then you should use one of the methods defined in _Table_ such as _take_.

## Other String Rendering

Apart from CSV, there is currently only one implementation of _String_ rendering, and that is _Json_ rendering.
Although Json is indeed a hierarchical serialization format, the manner of creating a Json string masks the hierarchical aspects.
The implemented Json reader/writer is Spray Json but that could easily be changed in the future.

Although this section is concerned with rendering, it is also true of course to say that tables can be read from Json strings.

The following example from _JsonRendererSpec.scala_ shows how we can take the following steps
(for the definitions of _Player_, _Partnership_, please see the spec file itself):
* read a table of players from a list of Strings (there are, as shown above, other signatures of parse for files, URLs, etc.);
* convert to a table of partnerships;
* write the resulting table to a Json string;
* check the accuracy of the Json string;
* check that we can read the string back in as a table.


    val strings = List("First, Last", "Adam,Sullivan", "Amy,Avagadro", "Ann,Peterson", "Barbara,Goldman")
    val wy: IO[String] = for (pt <- Table.parse[Table[Player]](strings)) yield Player.convertTable(pt).asInstanceOf[Renderable[Partnership]].render
    wy should matchPattern { case Success("{\n  \"rows\": [{\n    \"playerA\": \"Adam S\",\n    \"playerB\": \"Amy A\"\n  }, {\n    \"playerA\": \"Ann P\",\n    \"playerB\": \"Barbara G\"\n  }],\n  \"header\": [\"playerA\", \"playerB\"]\n}") => }
    implicit val r: JsonFormat[Table[Partnership]] = new TableJsonFormat[Partnership] {}
    wy.map(p => p.parseJson.convertTo[Table[Partnership]]) should matchPattern { case Success(HeadedTable(_, _)) => }

Release Notes
=============

V1.1.2 -> V1.1.3
* Use of Cats IO
* Table contents are now parallelized
* Option of having sequential rows of user type
* Improved Analysis by allowing Histogram

V1.1.1 -> V1.1.2
* Make RawRow a type (not just a type alias)

V1.1.0 -> V1.1.1
* Enable cryptographic capabilities
* Uses TSEC-JCA and Cats IO.
* Many relatively minor fixes/improvements.

V1.0.15 -> V1.1.0
* Enable CSV-rendering and selection of table rows.

V1.0.14 -> V1.0.15
* Minor changes

V1.0.13 -> V1.0.14
* Enabled multi-line quoted strings: if a quoted string spans more than one line, this is acceptable.
* Implemented analysis of raw-row tables.
* Implemented sampling of input.
* Provided a new mechanism for configuring and using parsers (see the worksheets).
* Implemented _Table.parseResourceRaw_ and _Table.parseFileRaw_ for those situations where you just want to parse an input file into a _Table\[Seq\[String]]_.

V1.0.12 -> V1.0.13
* mostly concerned with publishing TableParser in Maven Central

V1.0.11 -> V1.0.12
* mostly internal refactoring: restored Renderable interface (though different from before)

V1.0.10 -> V1.0.11
* introduction of logging;
* introduction of JSON (spray) for read/write of _Table_;
* _Table_ now supports _Iterable=>Iterable_ methods.
* renaming of Renderer to _HierarchicalRenderer_ and introduction of StringRenderer
* introduction of _TableParserHelper_;
* renamed _TableWithoutHeader_ as _UnheadedTable_ and _TableWithHeader_ as _HeadedTable_;
* added various methods, inc. _replaceHeader_, to _Table_.
* Table parsing is now based on _Iterator_ rather than _Iterable_.
* Table rows are now based on _Vector_ (at least for the standard _TableWithHeader_)

V1.0.9 -> V1.0.10
* build.sbt: changed scalaVersion to 2.13.3
* added StringTableParserWithHeader;
* now column names are found by case-independent comparison.

V1.0.8 -> V1.0.9
* build.sbt: changed scalaVersion to 2.12.10

V1.0.7 -> V1.0.8
* build.sbt: changed scalaVersion to 2.12.9
* refactored the concept of tables with/without headers in TableParser;
* enabled program-defined headers that match Excel-style numbers or letters.

V1.0.6 -> V1.0.7
* build.sbt: changed scalaVersion to 2.12.8
* CellParser: parametric type _T_ is now covariant;
* CellParsers: added new method _cellParserOptionNonEmptyString_;
    then for each of the _cellParserN_ methods, the signature has had a defaultable _fields_ parameter to allow explicit field naming;
* Reflection: changed the message to refer to the _cellParserN_ signatures;
* README.md: fixed some issues with the doc regarding the _MovieTableParser_;
    added new features above.
    
V1.0.5 -> V1.0.6
* Added a standard implicit value of _ColumnHelper_ for situations that don't need extra help.

V1.0.4 -> V1.0.5
* Added a convenient way of rendering a table as a non-hierarchical structure. In other words, serialization to a CSV file.

V1.0.3 -> V1.0.4
* Added the ability to add header row and header column for tables (NOTE: not finalized yet, but functional).

V1.0.2 -> V1.0.3
* Added no implicit warnings
* Created mechanism for rendering the result of parsing in a hierarchical structure.

V1.0.1 -> V1.0.2
* Added self-checking of _LineParser_;
* Able to parse two quote-chars together in a quotation as one quote char;
* Added enc and codec params as appropriate to _Table.parse_ methods.
* Added _stringCellParser_;
* Now, properly closes source in _Table.parse_ methods.

V1.0.0 -> V.1.0.1
* Fixed Issue #1;
* Added parsing of _Seq\[Seq\[String]]_;
* Added _cellParserRepetition_;
* Implemented closing of Source in _Table.parse_ methods;
* Added encoding parameters to _Table.parse_ methods.


