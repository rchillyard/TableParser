[![Codacy Badge](https://api.codacy.com/project/badge/Grade/1dc65c7cf84e46bfbb0d3d9b16c0f382)](https://app.codacy.com/app/scalaprof/TableParser?utm_source=github.com&utm_medium=referral&utm_content=rchillyard/TableParser&utm_campaign=Badge_Grade_Settings)
[![Maven Central](https://img.shields.io/maven-central/v/com.phasmidsoftware/tableparser-core_2.13.svg?color=blue&label=maven-central%20(2.13%2C%201.x))](https://central.sonatype.com/artifact/com.phasmidsoftware/tableparser-core_2.13)
[![Maven Central](https://img.shields.io/maven-central/v/com.phasmidsoftware/tableparser-core_3.svg?color=blue&label=maven-central%20(3))](https://central.sonatype.com/artifact/com.phasmidsoftware/tableparser-core_3)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/TableParser)
![GitHub](https://img.shields.io/github/license/rchillyard/TableParser)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/TableParser)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/TableParser)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/TableParser/bug)
[![RFC 4180](https://img.shields.io/badge/CSV-RFC%204180-brightgreen)](https://www.rfc-editor.org/rfc/rfc4180)

# Introduction to TableParser

> **RFC 4180 compliant.** From version 2.0.0, _TableParser_ reads and writes CSV exactly as defined by
> [RFC 4180](https://www.rfc-editor.org/rfc/rfc4180), the standard for CSV files.
> Every rule of the RFC is tested in [Rfc4180Spec](core/src/test/scala/com/phasmidsoftware/tableparser/core/Rfc4180Spec.scala),
> and a table that _TableParser_ writes is read back exactly (except that a line break inside a field is always read as LF).
> In particular:
> * Fields are separated by commas, and spaces are part of a field (so `a, b` has the fields `a` and ` b`).
> * A field may be quoted, in which case it may contain commas, line breaks and quotes (which are doubled: `"say ""hi"""`).
> * When writing, a field is quoted only when it has to be (when it contains a comma, a quote or a line break, or starts or ends with a space).
> * Records end with CRLF when written; CRLF or LF are both read.
>
> To read data whose fields are separated by `", "` (for example, written by 1.x with `CsvAttributes(", ")`), use `RowConfig.trimmingRowConfig`, which ignores spaces next to commas, as 1.x did.
> To write LF line endings instead of CRLF, use `CsvAttributes(",", "\"", "\n")`.

Full Scaladoc is available via javadoc.io, for [Scala 3](https://javadoc.io/doc/com.phasmidsoftware/tableparser-core_3)
(and, for the 1.x line, [Scala 2.13](https://javadoc.io/doc/com.phasmidsoftware/tableparser-core_2.13)).

A functional parser of tables, implemented in Scala.
Typically, the input is in the form of a "CSV" (comma-separated-values) file.
However, it is perfectly possible to parse other formats.
Indeed, as of 1.3.0, it is possible to parse Parquet files.
Please see the **[TableParser Parquet Design](docs/design/TableParserParquetDesign.md)**
design document for more information.

_TableParser_ aims to make it as simple as possible to ingest a fully typed tabular dataset.
The principal mechanism for this is the use of case classes to specify the types of fields in the dataset.
All conversions from strings to standard types are performed automatically,
and the parsers (and renderers) of case classes are derived by the compiler, with no limit on the number of fields.
For non-standard types, it suffices simply to provide a converter of the form _String=>T_.

It is possible to parse sequences of _String_ (one per row)--
the typical situation for a CSV file--or sequences of
_String_ (where the table corresponds to a matrix of cells).
 
This library makes extensive use of type classes (given instances, and derivation from a case class's _Mirror_).
Indeed, it is implemented very similarly to JSON readers.
There is a row-parser configuration (_RowConfig_) that allows
the programmer to vary the delimiter, the quote character, and how lists within a cell are written.

In addition to parsing,
_TableParser_ provides a mechanism for rendering a table in _hierarchical_ form (for example for
XML or HTML).
An output structure which is itself tabular or sequence-oriented can be generated
quite easily using the rows of the table, together with something like, for instance, a JSON writer.

Package Structure
===========

As of version 2.0.0, the code is made up of six modules (each published as _tableparser-<module>_):
* _core_: parsing and rendering tables. Most of the remainder of this README file refers to the _core_ module.
* _cats_: for use with Cats Effect, particularly _IO_ (see _IOTable_).
* _zio_: for use with ZIO (see _ZIOTable_).
* _crypto_: encryption of tables (e.g., of the rows of a CSV file), using tsec and Cats Effect (new in 2.0.0: in 1.x, this was part of _cats_).
* _json_: reading and writing tables as JSON, using circe (new in 2.0.0: in 1.x, JSON support, using spray-json, was part of _core_).
* _parquet_: for use with Apache Parquet (beginning with 1.3.0).

The _spark_ module (for use with Apache Spark) is on the 1.x line only, because Spark is not published for Scala 3.

Quick Intro
===========

The simplest way to get an introduction to _TableParser_ is to consult the _movie.sc_.
and _airbnb.sc_ worksheets (the latter is in the _cats_ package).
These give detailed descriptions of each stage of the process.

Another way to see how it works is to look at this application Pairings which takes a CSV file,
parses it, transforms the data,
and outputs a JSON file.
This way of parsing is a little different from what is shown in the worksheets.
But both are effective.
The minimum code necessary to parse the CSV file as a table of "Player"s,
using as many defaults as possible is:

    case class Player(first: String, last: String)

    object Player extends TableParserHelper[Player]() {
      def cellParser: CellParser[Player] = CellParser.derived[Player]
    }

    val pty: Try[Table[Player]] = Table.parseFile("players.csv")

The _TableParserHelper_ used here is an abstract subclass of _CellParsers_,
and is customized for the row type (in this case, _Player_).
In particular, it defines an implicit _TableParser\[Table\[X]]_,
where _X_ is the row type (_Player_ in this example).
This assumes that the source input file ("players.csv")
contains a header row which includes column names corresponding to the parameters
of the case class _Player_ (in this case "first" and "last").
If, for example, your CSV file does not have a header row,
then you make a minor change to the line _object Player..._

The input file looks something like this (the first and last columns are required,
others are ignored):

    Id,First,Last,
    1,Adam,Sullivan,
    2,Amy,Avagadro,
    3,Anna,Peterson,

etc...

Note that columns not needed for the _Player_ case class are simply ignored.
Also, note that a column whose name matches exactly is preferred; only if there is none is the case of the column names ignored.

For another simple use case _TableParser_, please see my blog at: https://scalaprof.blogspot.com/2019/04/new-projects.html 

# User Guide

This version of the README.md file refers to version: 2.0.2.

From version 2.0.0, _TableParser_ is for Scala 3 only (it is built with Scala 3.3 LTS, so it can be used from any later Scala 3 version),
and it needs Java 21 or later.
For Scala 2.13 (or Java 17), and for the spark module, use the 1.x line (the latest is 1.7.2), which is on the _1.x_ branch.

To use it, add the modules you need to your _build.sbt_:

```scala
libraryDependencies ++= Seq(
    "com.phasmidsoftware" %% "tableparser-core" % "2.0.2"
  , "com.phasmidsoftware" %% "tableparser-cats" % "2.0.2"    // cats-effect IO support
  , "com.phasmidsoftware" %% "tableparser-zio" % "2.0.2"     // ZIO support
  , "com.phasmidsoftware" %% "tableparser-crypto" % "2.0.2"  // encryption
  , "com.phasmidsoftware" %% "tableparser-json" % "2.0.2"    // JSON (circe)
  , "com.phasmidsoftware" %% "tableparser-parquet" % "2.0.2" // Parquet files
)
```

If you are moving from 1.x, see [Migrating from 1.x](#migrating-from-1x) below.
See the release notes (at the end) for history.

## Migrating from 1.x

2.0.0 is for Scala 3, and makes the most of it: almost everything that was written out by hand for each case class in 1.x is now derived by the compiler.
In the table below, "1.x" is what you wrote for 1.7, and "2.0" is what replaces it.

| Topic | 1.x | 2.0 |
|---|---|---|
| Scala and Java | Scala 2.13 or 3; Java 17 | Scala 3 only (3.3 LTS or later); Java 21 |
| Parsing a case class | `cellParser2(apply)` ... `cellParser13(apply)` (at most 13 fields) | `CellParser.derived[T]` (or `derives CellParser`), with no limit on the number of fields |
| Rendering a case class as CSV | `renderer2(apply)`, `generator2(apply)` ... (at most 19 fields) | `CsvRenderer.derived[T]`, `CsvGenerator.derived[T]`, or both at once with `derives CsvProduct` |
| Hierarchical rendering of a case class | `renderer2("tr")(apply)` etc. | `HierarchicalRenderer.derived[T]("tr")` |
| Parquet: grouped case classes | `converter2(apply)` ... `converter8(apply)`, and `converterMap`/`groupedHelpers` | derived automatically (`ParquetCellConverter.derived`): each case class needs only its `ColumnHelper` |
| Field names | found by reflection (`Reflection.extractFieldNames`, with a `ClassTag`) | from the case class's `Mirror`, at compile time (the `FieldNames[T]` type class); `Reflection` is gone |
| Implicits | `implicit val`, `implicitly` | `given`, `summon` (an `implicit val` still works, of course). Note that a wildcard import doesn't bring givens: use `import X.{given, *}` |
| Configuring how rows are read | `implicit object C extends DefaultRowConfig { override val delimiter = """\t""".r; override val string = ... }` | `given RowConfig = RowConfig(delimiter = '\t')`: `RowConfig` is a case class, whose regexes are derived from its characters (`withDelimiter` etc. change one property). `DefaultRowConfig` is gone |
| CSV format | spaces next to a comma were ignored; LF line endings | RFC 4180: spaces are part of a field (use `RowConfig.trimmingRowConfig` for 1.x behavior); CRLF when writing (`CsvAttributes(",", "\"", "\n")` for LF) |
| Reading and writing in the same format | `RowConfig` and `CsvAttributes` were unrelated | `rowConfig.csvAttributes` and `csvAttributes.rowConfig` |
| Column names when writing | the field names (with dots for nested case classes) | the names the parser looks up (via `ColumnHelper`), so a table that is written can be read back |
| Row order | `Content` was a `ParIterable` (rows could come back in any order) | `Content` is a `Vector`, in the order the rows were read; `content.par` for parallelism |
| Dates | Joda-Time (`org.joda.time.LocalDate`) | `java.time.LocalDate` |
| JSON | spray-json, in core (`TableJsonFormat`) | circe, in the new _tableparser-json_ module (`TableCodecs`) |
| Encryption | in _tableparser-cats_ | in the new _tableparser-crypto_ module (package `com.phasmidsoftware.tableparser.crypto`) |
| `MaybeStatistics` | a sealed trait | an enum |
| `Sequential`, `Sequence`, the examples, `table.Main`, `EvaluateZIO` | deprecated in 1.7.0 | removed (the examples are now in the tests) |
| A blank `Option[URL]` cell | a failure | `None`, as for every other type |
| `StringConverter`'s instances, `Header.multiply`/`prepend`/`alphabet` | public | removed (unused) |
| Names ending in `$` (e.g. `BooleanCellParser$`) | as in 1.x | the `$` is dropped (e.g. `BooleanCellParser`) |

## Parsing

Parsing Row-oriented Tables
===========================

The _Table_ trait expresses the result of parsing from a serialized representation of a table.
Each row is represented by a parametric type _Row_.
Typically,
this _Row_ type is a case class with one parameter corresponding to one column in the table file.
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

This analysis will give you a list of columns, each showing its name,
whether it is optional (i.e., contains nulls), and (if it's a numerical column),
its range, mean, and standard deviation.

Incidentally, this raw parser has three signatures, one for resources, one for files,
and one for a sequence of Strings.
And the default for raw row parsing is to allow quoted strings to span multiple lines.

But, if not parsing as raw rows,
you will need to design a class hierarchy to model the columns of the table.
_TableParser_ will take care of any depth of case classes.
There is no limit on the number of parameters of a case class
(see _UnstructuredMovie_, in the tests, which has a field for each of the 28 columns of the movie database).

The names of the parameters of a case class do not necessarily
have to be the same as the column from which the value derives.
The _ColumnHelper_ class is available to manage the mapping between parameters and columns.

The result of parsing a table file (CSV, etc.) will be a _Table\[Row]_, wrapped in _Try_.
There are object methods to parse most forms of text:
_File, Resource, InputStream, URL, Seq\[String]_, etc. (see _Table_ below).

The parser responsible for parsing the contents of a cell is called _CellParser\[T]_,
where _T_ is the type of the value
in the cell in question.
_T_ is covariant so that if you have alternative parsers which generate different subclasses of trait,
for instance, this can be done.

In order for _TableParser_ to know how to construct a case class from a set of values,
an instance of _CellParser\[T]_ must be in scope.
It is derived from the case class (using its _Mirror_):

    given CellParser[T] = CellParser.derived[T]

or, equivalently, `case class T(...) derives CellParser`.
The parsers of the fields come from their own types (each must have a _CellParser_),
and a field which is itself a case class (with no _CellParser_ of its own) is derived in turn.
Each field is read from the column whose name is given by the _ColumnHelper\[T]_ for that field's name.

NOTE: for a case class which is defined locally (inside a method),
don't derive its _CellParser_ inside a companion object that you write for it:
initializing that object would refer to itself (a _StackOverflowError_).
Use `derives CellParser`, or define the given alongside the case class.

See the section on _CellParsers_ below.

Parsing Column-oriented Tables
==============================

### Parquet parsing

The mechanisms described above are primarily designed for parsing row-oriented tables such as CSV.
However, as of version 1.3.0, it is possible to parse Parquet files.
Please see the **[TableParser Parquet Design](docs/design/TableParserParquetDesign.md)**
design document for more information.       

Much of the core functionality is applicable to column-oriented tables as well.
The most significant differences are 
- that the header is always known and is defined by the Parquet file itself.
- the concept of the RowParser does not translate to column-oriented tables.

For an example of how to parse a Parquet file,
see the _YellowTaxiTrip_ example in **[YellowTaxiTrip](parquet/src/test/scala/com/phasmidsoftware/tableparser/parquet/YellowTaxiTrip.scala).
This is from the NYC Taxi dataset.

## Table

The _Table_ class, which extends _Iterable\[Row]_, also has several methods for manipulation:
### query methods

* def content: Content\[Row]
* def maybeHeader: Option\[Header]
* def toCSV(using renderer: CsvRenderer\[Row], generator: CsvGenerator\[Row], csvAttributes: CsvAttributes): Try\[String]
* def maybeColumnNames: Option\[Seq\[String]]
* def column(name: String): Iterator\[Option\[String]]
* def writeCSVFile(path: Path)(using renderer: CsvRenderer\[Row], generator: CsvGenerator\[Row], csvAttributes: CsvAttributes): Unit

To write a table encrypted, see _TableCrypt.writeCSVFileEncrypted_ in the _crypto_ module.

### transformation methods
* def filter(p: Row => Boolean): Table\[Row]
* def filterNot(p: Row => Boolean): Table\[Row]
* def filterValid(using rv: Validity\[Row]): Table\[Row]
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
* def sample(n: Int)(using random: Random): Table\[Row]
* def slice(from: Int, until: Int): Table\[Row]
* lazy val shuffle: Table\[Row]


It is to be expected that _join_ methods will be added later
(based upon the second signature of processRows).

The following **object** methods are available for parsing text:
*  def parse\[T: TableParser](ws: Iterable\[String]): Try\[T]
*  def parse\[T: TableParser](ws: Iterator\[String]): Try\[T]
*  def parseSource\[T: TableParser](x: => Source): Try\[T]
*  def parse\[T: TableParser](sy: => Try\[Source]): Try\[T]
*  def parse\[T: TableParser](u: => URI)(using codec: Codec): Try\[T]
*  def parse\[T: TableParser](u: => URI, enc: String): Try\[T]
*  def parseInputStream\[T: TableParser](i: => InputStream)(using codec: Codec): Try\[T]
*  def parseInputStream\[T: TableParser](si: Try\[InputStream], enc: String): Try\[T]
*  def parseFile\[T: TableParser](path: Path)(using codec: Codec): Try\[T]
*  def parseFile\[T: TableParser](path: Path, enc: String): Try\[T]
*  def parseResource\[T: TableParser](w: String, clazz: Class\[?] = getClass)(using codec: Codec): Try\[T]
*  def parseResource\[T: TableParser](u: => URL)(using codec: Codec): Try\[T]
*  def parseResource\[T: TableParser](u: => URL, enc: String): Try\[T]
*  def parseSequence\[T: TableParser](wss: Iterator\[Seq\[String]]): Try\[T]

and, for raw tables (of _RawRow_): _parseRaw_, _parsePathRaw_, _parseFileRaw_ and _parseResourceRaw_.
_IOTable_ (in the _cats_ module) and _ZIOTable_ (in the _zio_ module) have the same methods, giving an _IO_ or a _Task_.

Please note that, in the case of a parameter being an Auto-closeable object such as _InputStream_ or Source,
it is the caller's responsibility to close it after parsing.
However, if the parameter is a _Path_, _File_, or filename, or _URL_/_URI_,
then any Source object that is instantiated within
the parse method will be closed.
This applies also to the _parseInputStream_ methods:
the internally defined _Source_ will be closed (but not the stream).

Additionally, there is an extension method _parse_ (defined in the _TableParser_ companion object)
which allows for expressions such as:

    parser parse source

This is the recommended way to parse because it is the simplest.
It also allows chaining of copy (or "lens") methods to configure the parser, for example:

    val parser = RawTableParser().setPredicate(TableParser.sampler(2)).setMultiline(true)

## TableParser

_TableParser_ is also the name of a trait that takes a parametric type called "Table" in its definition.
This is NOT the same at the _Table_ type (described above).
_TableParser_ is defined thus:

    trait TableParser[Table] {
      type Row
      type Input
      protected val maybeHeader: Option[Header] = None
      val headerRowsToRead: Int = 1
      def forgiving: Boolean = false
      def multiline: Boolean = false
      val predicate: Try[Row] => Boolean = includeAll
      def rowParser: RowParser[Row, Input]
      def builder(rows: Iterator[Row]): Table
      def parse(xs: Iterator[Input], n: Int = headerRowsToRead): Try[Table]
}

The type _Row_ defines the specific row type of the resulting _Table_
(for example, _Movie_, in the example below).
The type _Input_ defines the input type, typically _String_,
but there are also alternatives such as _Seq\[String]_.
_maybeHeader_ is an optional parameter that, if defined,
specifies the (fixed) header to be used for parsing.
In such a case, the system will not look for a header row(s) in the input.
_headerRowsToRead_ is the number of header rows to be read,
as sometimes it extends beyond one line.
_forgiving_, which defaults to _false_,
can be set to _true_ if you expect that some rows will not parse, but where this
will not invalidate your dataset as a whole.
_multiline_ is used to allow (or disallow when false) quoted strings to span multiple lines.

In forgiving mode, any exceptions thrown in the parsing of a row are collected and then logged.
_rowParser_ is the specific parser for the _Row_ type (see below).
_builder_ is used by the _parse_ method.
_parse_ is the main method of _TableParser_ and takes an _Iterator\[Input]_ and yields a _Try\[Table]_.
You can also specify the number of header rows to be read, but this defaults to _headerRowsToRead_.

The _predicate_ is used to filter rows (which are the results of parsing).
By default, all rows are included.
_TableParser_ also provides a method (_sampler_) to create a random sampling function.
Note, however,
that a significant part of the time for building a table from a large file is just reading and parsing the file.
Sampling will not reduce this portion of the time.

Associated with _TableParser_ is an abstract class called _TableParserHelper_,
whose purpose is to make your coding job  easier.
_TableParserHelper_ is designed to be extended i.e., subclassed,
by the companion object of the case class that you
wish to parse from a row of your input.
Doing it this way makes it easier for the implicit TableParser instance to be found.
You can also set up your application along the lines of the examples below,
such as the Movie example.

The constructor for _TableParserHelper_ takes two parameters, both of which can be defaulted:
* sourceHasHeaderRow: Boolean = true
* forgiving: Boolean = false

## RowParser

_RowParser_ is a trait that defines how a line of text is to be parsed as a _Row_.
_Row_ is a parametric type that, in subtypes of _RowParser_, is context-bound to _CellParser_.
A second parametric type _Input_ is defined:
this will take on values of _String_ or _Seq\[String]_, according to the form of input.
Typically, the _StandardRowParser_ is used,
which takes as its constructor parameter a _LineParser_.

The methods of _RowParser_ are:

    def parse(header: Header)(w: String): Try[Row]

    def parseIndexed(header: Header)(indexedRow: (Input, Int)): Try[Row]

    def parseHeader(w: String): Try[Header]

The parseIndexed method is useful when we care about the sequential aspect of the input.
This is particularly important if strings are allowed to spread over newlines
(as in the Airbnb dataset).

## LineParser

The _LineParser_ takes five parameters: two regexes, a String and two Chars.
These define, respectively, the delimiter regex, the string regex, list enclosures,
the list separator, and the quote character.
Rather than invoke the constructor directly,
it is easier to invoke the companion object's _apply_ method,
which takes a single _using_ parameter: a _RowConfig_.

_RowConfig_ is a case class of characters, from which the regexes are derived:

    case class RowConfig(delimiter: Char = ',', quote: Char = '"', listSep: Char = '|', listEnclosure: String = "{}",
                         trim: Boolean = false, maybeString: Option[Regex] = None)

The default (_RowConfig.default_) reads RFC 4180.
To read, for example, tab-separated values, define `given RowConfig = RowConfig(delimiter = '\t')`
(or `RowConfig.default withDelimiter '\t'`): the regex for an unquoted field changes with the delimiter.
If _trim_ is true (as in _RowConfig.trimmingRowConfig_), spaces next to a delimiter are ignored, as in 1.x.
_rowConfig.csvAttributes_ gives the _CsvAttributes_ which write tables in the same format (and _csvAttributes.rowConfig_ the reverse).
Two consecutive quote characters, within a quoted string,
will be parsed as a single quote character.
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

* def rawRowCellParser: CellParser\[RawRow]
* def cellParserRepetition\[P: CellParser](start: Int = 1): CellParser\[Seq\[P]]
* def cellParserSeq\[P: CellParser]: CellParser\[Seq\[P]]
* def cellParserOption\[P: CellParser]: CellParser\[Option\[P]]
* lazy val cellParserOptionNonEmptyString: CellParser\[Option\[String]]
* def cellParser\[P: CellParser, T](construct: P => T): CellParser\[T]
* inline def cellParser2Conditional\[K, P, T](construct: (K, P) => T, parsers: Map\[K, CellParser\[P]]): CellParser\[T]
* def columnHelper\[T](maybePrefix: Option\[String], aliases: (String, String)*): ColumnHelper\[T]
* etc. including other ways to instantiate a ColumnHelper\[T].

The parser of a case class itself is derived: see _CellParser.derived_ above
(it replaces 1.x's _cellParser1_ ... _cellParser13_).

There is one additional method to handle the situation where you want to vary the parser
for a set of cells according
to the value in another (key) column: _cellParser2Conditional_.
In this case, you must supply a _Map_,
which specifies which parser is to be used for each possible value of the key column.
If the value in that column is not one of the keys of the map, an exception will be thrown.
For an example of this, please see the example in _CellParsersSpec_ ("conditionally parse").

### Implicits

Keep in mind when using given (implicit) values that the best practice
is to define a given involving a type T,
for example, _CellParser\[T]_, in the companion object of _T_.
This will tend to eliminate any ambiguously defined implicits,
and it also tends to avoid any problems with  initialization.
If you still run into initialization problems, try defining the troublemaker as lazy.
It also relieves you from having to make up names for the implicit values
(which the compiler more or less ignores, anyway).
Just ensure that the name is valid, doesn't invoke a recursion, and is not in conflict with another name.
If you look in the example of _Principal_ (below),
you will see that this is also the place to define optional parsers, sequential parsers, etc.

## Content

The rows of a _Table_ are represented by a case class called _Content_:

    case class Content[+Row](private val xs: Vector[Row])

The rows are kept in the order they were read.
(In 1.x, they were held in a _ParIterable_, which shuffled them.)
For parallel processing, _content.par_ gives the rows as a parallel collection.

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

        import Movie._
        val x: Try[Table[Movie]] = Table.parseResource[Table[Movie]]("movie_metadata.csv", classOf[Movie])
     
In this example, the row type is _Movie_, a case class with eleven parameters.
The data can be found in a local resource (relative to this class) called movie_metadata.csv.
All the (given) details that characterize this particular table input are provided
in the _Movie_ companion object.

The _Movie_ class looks like this:

    case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Option[Principal], genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

Note that we make _actor3_ optional because some movies don't specify an "actor3".

In order to parse a _Movie_, we will need to declare some implicit values in the companion object.
The following is the required code:

    object Movie extends CellParsers with CsvGenerators with CsvRenderers {
        val missing: Movie = apply("", Format.none, Production.none, Reviews.none, Principal.nemo, Principal.nemo, Principal.nemo, None, AttributeSet.none, AttributeSet.none, "")
        val header = "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes"
        implicit val helper: ColumnHelper[Movie] = columnHelper(camelToSnakeCaseColumnNameMapperLower,
            "title" -> "movie_title",
            "imdb" -> "movie_imdb_link")
        implicit val parser: CellParser[Movie] = CellParser.derived[Movie]
        implicit val renderer: CsvRenderer[Movie] = CsvRenderer.derived[Movie]
        implicit val generator: CsvGenerator[Movie] = CsvGenerator.derived[Movie]

        // Additional code shown below for parsing tables or processing rows.
    }

We define a missing object because that is sometimes convenient to use with Spark.
Similarly, the header object is the standard header strings,
which can be used when reading a CSV file without a header.
The (implicit) _helper_ is used to map the names of columns appropriately.
The (implicit) _parser_ is the required _CellParser_ for _Movie_.
The (implicit) _renderer_ is used to render a _Movie_ as a CSV file.
The (implicit) _generator_ is used for outputting a _Movie_ in other format(s).

Each of the case classes referenced in the declaration of _Movie_,
will also need a similar companion object.
For example, the _Principal_, whose case class is defined thus:

    case class Principal(name: Name, facebookLikes: Int)

requires a companion object that looks like this:

    object Principal extends CellParsers with CsvGenerators with CsvRenderers {
        val nemo: Principal = Principal(Name.nemo, 0)
        implicit val helper: ColumnHelper[Principal] = columnHelper(camelToSnakeCaseColumnNameMapperLower, Some("$x_$c"))
        implicit val parser: CellParser[Principal] = CellParser.derived[Principal]
        implicit val parserOpt: CellParser[Option[Principal]] = cellParserOption
        implicit val renderer: CsvRenderer[Principal] = CsvRenderer.derived[Principal]
        implicit val generator: CsvGenerator[Principal] = CsvGenerator.derived[Principal]
        // An optional Principal (e.g. actor3) has the same columns as a Principal: when it is missing, they are empty.
        implicit val rendererOpt: CsvRenderer[Option[Principal]] = optionProductRenderer[Principal]
        implicit val generatorOpt: CsvGenerator[Option[Principal]] = optionProductGenerator[Principal]
    }

Like _Movie_, it has a default value (_nemo_), as well as a _helper_ to get the column names correct.
We need to define parsers, renderers, etc. for optional *Principal*s.
Unlike with primitive values such as _Int_, _Double_, we do have to add additional
_implicit_ definitions to accomplish this.
Note that _optionProductRenderer_ (and _optionProductGenerator_) are for an optional case class, which has several columns:
_None_ is written as the right number of empty cells.
For an optional value with one column, use _optionRenderer_, which takes an optional parameter that defines a _String_ to be used when the value
is missing.
 
The other case classes look like this:

    case class Format(color: String, language: String, aspectRatio: Double, duration: Int)
    case class Production(country: String, budget: Option[Int], gross: Int, titleYear: Int)
    case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Int, numUsersVoted: Int, numCriticReviews: Int, totalFacebookLikes: Int)
    case class Name(first: String, middle: Option[String], last: String, suffix: Option[String])
    case class Rating(code: String, age: Option[Int])

Consult the actual code in _Movie.scala_ for the details
of what is required in the corresponding companion objects.

The _Movie_ object has additional code like this:

    object Movie ... {
        // Required code as shown above

        // First, we need a StringParser[Movie]...
        implicit val movieConfig: RowConfig = RowConfig(listEnclosure = "")
        implicit val stringParser: StringParser[Movie] = StandardRowParser.create[Movie]

        // Next, if we want to be able to parse rows into a Table[Movie], we will need a TableParser[Movie]...
        trait MovieTableParser extends HeadedCSVTableParser[Movie] {
            override val forgiving: Boolean = true
        }
        implicit object MovieTableParser extends MovieTableParser
    }

We use the `forgiving` mode for `MovieTableParser` because we expect
that there will be many rows which cannot be parsed.
In this code, _helper_, and the other columnHelpers, specify parameter-column mappings.
Note that _helper_ for _Principal_ has an extra parameter at the start of the parameter list:
    
    Some("$x_$c")
    
which is an (optional) formatter for the purpose of prefixing a string to column names.
That's because there are several "Principal" parameters in a _Movie_,
and each one has its own set of attributes. 
In this format parameter, "$x" is substituted by the prefix
(the optional value passed into the lookup method)
while $c represents the translated column name.

A couple of parameters of _Movie_ are actually attribute sets (_AttributeSet_).
These are basically lists of _String_ within one column value.
Such lists are parsed as lists as they are parsed from the original strings
and then returned as strings
in the form "{" element "," element ... "}"
The parsing from the original string obeys the _RowConfig_ parameters of _listSep_ and _listEnclosure_.

In this case, the row config is defined as _movieConfig_, whose lists are not enclosed (in 1.x, they were enclosed in "{}" by default).
A _RowConfig_ also defines the _delimiter_ character which separates the columns,
and the _quote_ character which can be used to include cell values which contain the delimiter.

A parameter can be optional, for example, in the _Movie_ example, the _Production_ class is defined thus:

    case class Production(country: String, budget: Option[Int], gross: Int, title_year: Int)
    
In this example, some movies do not have a budget provided.
All you have to do is declare it optional in the case class,
and _TableParser_ will specify it as _Some(x)_ if valid, else _None_.

Note that there is a default given _RowConfig_ (_RowConfig.default_) defined in the object _RowConfig_.

If, instead of building a _Table\[Movie]_, you prefer to process rows into an _Iterator\[Movie]_,
then you should define the following instead of the _MovieTableParser_:

    object Movie ... {
        // Required code as shown above...

        // Finally, an optional row processor--this is useful when you simply want to end up with an _Iterator[Movie]_
        // rather than a Table[Movie]. Typically, we will either use this OR MovieTableParser.
        trait MovieRowProcessor extends StringRowProcessor[Movie] {
            override val forgiving: Boolean = true
        }
        implicit object MovieRowProcessor extends MovieRowProcessor
    }

## Example: Submissions

This example has two variations on the earlier theme of the _Movies_ example:
(1) each row (a Submission) has an unknown number of _Question_ parameters;
(2) instead of reading each row from a single String, we read each row from a sequence of Strings,
each corresponding to a cell.

The example comes from a report on the submissions to a Scala exam.
Only one question is included in this example.

    case class Submission(username: String, lastName: String, firstName: String, questions: Seq[Question])

    object Submission extends CellParsers {
        implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(ColumnHelper.camelCaseColumnNameMapperSpace, Some("$c $x"))
        implicit val submissionParser: CellParser[Submission] = CellParser.derived[Submission]
        implicit val parser: StandardStringsParser[Submission] = StandardStringsParser[Submission]()

        implicit object TableParser extends StringsTableParser[Table[Submission]] {
          type Row = Submission

          protected def builder(rows: Iterable[Row], header: Header): Table[Row] = HeadedTable(rows, header)

          override val forgiving: Boolean = false

          val rowParser: RowParser[Row, Seq[String]] = summon[RowParser[Row, Seq[String]]]
        }
    }

    case class Question(questionId: String, question: String, answer: Option[String], possiblePoints: Int, autoScore: Option[Double], manualScore: Option[Double])

    object Question extends CellParsers {
        private val mapper: String => String = _.replaceAll("(_)", " ")
        implicit val helper: ColumnHelper[Question] = columnHelper(mapper, Some("$c $x"), "questionId" -> "question_ID")
        implicit val optParserString: CellParser[Option[String]] = cellParserOption
        implicit val parser: CellParser[Question] = CellParser.derived[Question]
        implicit val seqParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
    }

To test this example, we run a unit test as follows (using scalatest):

    behavior of "TableParser"
    it should "parse Submission" in {
        val rows: Seq[Seq[String]] = Seq(
          Seq("Username", "Last Name", "First Name", "Question ID 1", "Question 1", "Answer 1", "Possible Points 1", "Auto Score 1", "Manual Score 1"),
          Seq("001234567s", "Mr.", "Nobody", "Question ID 1", "The following are all good reasons to learn Scala -- except for one.", "Scala is the only functional language available on the Java Virtual Machine", "4", "4", "")
        )
        import Submission.TableParser
        matchTry(Table.parseSequence(rows.iterator)) {
          case rt@HeadedTable(_, _) =>
            println(rt.head)
            rt.size shouldBe 1
        }
    }

Note the use of _cellParserRepetition_. The parameter allows the programmer to define the start value of the sequence number for the columns.
In this case, we use the default value: 1 and so don't have to explicitly specify it.
Also, note that the instance of _ColumnHelper_ defined here has the formatter defined as "$c $x",
which is in the opposite order from the _Movie_ example.

Rendering
=========

_TableParser_ provides a general mechanism for rendering (serializing to text) tables.
Indeed, the tables built by the parsers extend _TableRenderable\[Row]_,
which supports the _render(using ev: Renderer\[Table\[Row], O])_ method. 
There are two distinct mechanisms for rendering a table:
* one to a straight serialized output, for example, when rendering a table as a CSV file.
* the other to a hierarchical (i.e., tree-structured) output, such as an HTML file.

## Non-hierarchical output

For this type of output,
the application programmer must provide an instance of _Writer\[O]_ which is, 
for example, a _StringBuilder_, _BufferedOutput_, or perhaps an I/O Monad.

The non-hierarchical output does not support the same customization
of renderings as does the hierarchical output.
It's intended more as a straight, quick-and-dirty output mechanism to a CSV file.

Here, for example, is an appropriate definition.

	given StringBuilderWriteable: Writable[StringBuilder] with {
		def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
		def unit: StringBuilder = new StringBuilder
		override def delimiter: CharSequence = "|"
	}
	
The default _delimiter_ is ", ".
You can override the _newline_ and _quote_ methods too if you don't want the defaults.	

And then, following this, you will write something like the following code:

    print(table.render.toString)

The _Writable_ object will take care of inserting the delimiter and quotes as appropriate.
Columns will appear in the same order as the parameters of _Row_ type
(which must be either a _Product_, such as a case class, or an _Array_ or a _Seq_).
If you need to change the order of the rows,
you will need to override the _writeRow_ method of _Writable_.
 
## Hierarchical rendering

A type class called _TreeWriter_ is the main type for hierarchical rendering.
One of the instance methods of _Table\[Row]_ is a method as follows:

    def renderHierarchical[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(using rr: HierarchicalRenderer[Row]): U
    
Providing that you have defined a given of type _TreeWriter\[U]_
and a _HierarchicalRenderer\[Row]_,
then the _renderHierarchical_ method will produce an instance of _U_ which
will be a tree containing all the rows of this table.

What sort of type is _U_?
An XML node would be appropriate.
The specifications use a type called HTML which is provided in package _core.render.tag_
more as an exemplar rather than something definitive.

    case class HTML(name: String, attributes: Seq[Attribute], content: Option[String], tags: Seq[Tag])

The example _TreeWriter_ for this type (from _HierarchicalRendererSpec_) is reproduced here:

    trait TreeWriterHTML extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.attributes.toSeq.map(kv => Attribute(kv)), node.content, node.children.toSeq map evaluate)
    }

    implicit object treeWriterHTML extends TreeWriterHTML

If we have a row type as, for example:

	case class Complex(r: Double, i: Double)
	
Then, we should define appropriate renderers along the following lines:

	implicit val valueRenderer: HierarchicalRenderer[Double] = renderer[Double]("td")
	implicit val complexRenderer: HierarchicalRenderer[Complex] = HierarchicalRenderer.derived[Complex]("tr")

We can then write something like:

	val table = HeadedTable(Seq(Complex(0, 1), Complex(-1, 0)), Header.create("r", "i"))
	val h = table.renderHierarchical("table", Map("border" -> "1"))
	 
The result of this will be an HTML tree that can be written out thus as a string:
	 
	 <table border="1">
     <tr>
     <td name="r">0.0</td>
     <td name="i">1.0</td></tr>
     <tr>
     <td name="r">-1.0</td>
     <td name="i">0.0</td></tr></table>

As with the parsing methods, 
the conversion between instances of types (especially case classes) and
Strings is hierarchical (recursive).

If you need to set HTML attributes for a specific type, for example, a row in the above example,
then an attribute map can be passed to _HierarchicalRenderer.derived_ (its second parameter).

## CSV Rendering

If you simply need to write a table to CSV (comma-separated value) format as a _String_,
then use the _toCSV_ method of _Table\[T]_.
Note that there is also an object method of _Table_ called _toCSVRow_,
which can be used for instances of _Table\[Row]_.
More control can be gained by using _CsvTableStringRenderer\[T]_ or _CsvTableFileRenderer\[T]_
for a particular type _T_.

These require customizable (implicit) evidence parameters and are defined as follows:

    case class CsvTableStringRenderer[T]()(using CsvRenderer[T], CsvGenerator[T], CsvAttributes) extends CsvTableRenderer[T, StringBuilder] ...
    case class CsvTableFileRenderer[T](path: Path)(using CsvRenderer[T], CsvGenerator[T], CsvAttributes) extends CsvTableRenderer[T, FileWriter] ...
    abstract class CsvTableRenderer[T, O]()(using CsvRenderer[T], CsvGenerator[T], Writable[O], CsvAttributes) extends Renderer[Table[T], O] {...}

_CsvRenderer\[T]_ determines the layout of the rows,
while _CsvGenerator\[T]_ determines the header.
_CsvAttributes_ specify the delimiter and quote characters for the output.
For a case class, these are derived: _CsvRenderer.derived\[T]_ and _CsvGenerator.derived\[T]_,
or `derives CsvProduct` for both at once (they replace 1.x's _renderer1_ ... _renderer19_ and _generator1_ ... _generator19_).
The column names come from the same _ColumnHelper_ that parsing uses, so a table that is written can be read back.
Other instances can be created using methods in _CsvRenderers_ and _CsvGenerators_ respectively:
* sequenceRenderer, optionRenderer, optionProductRenderer.
* sequenceGenerator, optionGenerator, optionProductGenerator.

In some situations,
you will want to omit values (and corresponding header columns) when outputting a CSV file.
You may use the following methods (from the same types as above):

    def skipRenderer[T](alignment: Int = 1)(using ca: CsvAttributes): CsvRenderer[T]
    def skipGenerator[T](using ca: CsvAttributes): CsvGenerator[T]
    
Note that, when rendering a CSV row, you may want to simply render some number of delimiters
(this would be in the case where you have a fixed header).
You can use the _alignment_ parameter of _skipRenderer_ to ensure the alignment is correct.

As usual,
the standard types are pre-defined for both _CsvRenderer\[T]_ and _CsvGenerator\[T]_ (for Int, Double, etc.).

The methods mentioned above render tables in the form of CSV Strings.
However, there are also methods available to render tables as a _File_:
_writeCSVFile_ and _writeCSVFileRow_.
These utilize the type _CsvTableFileRenderer\[T]_ mentioned above.

If you wish to output only a subset of rows,
then you should use one of the methods defined in _Table_ such as _take_.

## Other String Rendering

Apart from CSV, there is currently only one implementation of _String_ rendering,
and that is _Json_ rendering.
Although JSON is indeed a hierarchical serialization format,
the manner of creating a JSON string masks the hierarchical aspects.

JSON support is in its own module, _tableparser-json_, which uses [circe](https://circe.github.io/circe/),
so that _tableparser-core_ doesn't depend on a JSON library.
(1.x used spray-json, in core.)

Although this section is concerned with rendering, it is also true, of course,
to say that tables can be read from Json strings.
_TableCodecs_ provides a circe _Encoder_ and _Decoder_ for _Table[T]_, given an _Encoder_ (or _Decoder_) for _T_.
A table is a JSON object with a "rows" array and, if the table has a header, a "header" array of column names.

The following example, from _TableCodecsSpec.scala_ (in the json module), reads the movie database,
writes it as JSON, and reads the JSON back.
_UnstructuredMovie_ (in core's tests) is a case class with a field for each of the 28 columns,
many of them optional:

    import com.phasmidsoftware.tableparser.json.render.JsonTableRenderer
    import com.phasmidsoftware.tableparser.json.table.TableCodecs.given
    import io.circe.Codec
    import io.circe.parser.decode

    given Codec.AsObject[UnstructuredMovie] = Codec.AsObject.derived

    val movies: Table[UnstructuredMovie] = Table.parseResource[Table[UnstructuredMovie]]("movie_metadata.csv", classOf[UnstructuredMovie]).get
    val json: String = new JsonTableRenderer[UnstructuredMovie].render(movies)
    val decoded: Either[io.circe.Error, Table[UnstructuredMovie]] = decode[Table[UnstructuredMovie]](json)
    decoded.map(_.toList) shouldBe Right(movies.toList)

The JSON looks like this (abbreviated):

    {
      "rows" : [
        {
          "color" : "Color",
          "directorName" : "James Cameron",
          "numCriticForReviews" : 723,
          ...
          "gross" : 760505847,
          ...
        },
        ...
      ],
      "header" : [
        "color",
        "director_name",
        "num_critic_for_reviews",
        ...
      ]
    }

Note that the header and the keys of the rows differ.
The header is the table's own header: here, the column names of the CSV file.
The keys of each row are whatever its _Encoder_ writes: for a derived _Codec_, the field names.
A missing (_None_) value is written as _null_ (and a missing or _null_ value is read as _None_).
Reading JSON which is malformed, or which doesn't match the row type, gives a _Left_.
For more examples (numbers, dates, nested case classes, and bad input), see _TableCodecsSpec.scala_ and _JsonTableRendererSpec.scala_.

Release Notes
=============
V2.0.1 -> V2.0.2
* Encrypted tables (the crypto module) can be written and read again (both had been broken since the modules were split):
  * _TableCrypt.writeCSVFileEncrypted_ takes a key function (from the key of a row to its encryption key), as
    _EncryptedHeadedStringTableParser_ does, so the same key map writes and reads.
    It returns a _Try\[Unit]_: up to 2.0.1, it failed without saying so, and wrote nothing.
  * _Table.parse_ (and _parseFile_, _parseResource_, etc.) decrypts with an _EncryptedHeadedStringTableParser_:
    up to 2.0.1, it gave the encrypted rows. Its _parseIO_ no longer drops the first encrypted row.
  * New: _HexEncryption.encryptHex_ (the inverse of _decryptHex_) and _WritableCrypt.fileWritableCrypt_.
* A table whose rows are _List_s (or other _Seq_s) is rendered correctly: a row was written as, e.g., "1,List(2)".
* _LineParser_'s construction check finds the delimiter of _RowConfig_'s regexes (it took it to be "Q" or "s"),
  and no longer makes the constructor fail for a delimiter which has no single character.
* The code uses Scala 3's indentation syntax (a layout-only change).
* Tests: 542 (and 18 integration tests); statement coverage is 87.5% (branch coverage 81.3%).

V2.0.0 -> V2.0.1
* The cats and zio modules now behave as effects should:
  * _IOTable_ does its work when the _IO_ is run (and each time it is run), not when the _IO_ is made:
    _writeCSVFileIO_, for example, wrote the file even if the _IO_ was never run.
  * _ZIOTable_ closes the files (and streams) that it reads: it used to leave them open.
  * _ZIOTable_ reports a file that can't be opened as a failure (which _catchAll_ etc. can handle), not as a defect.
  * Reading and writing files runs on the blocking thread pool (_IO.blocking_, _ZIO.attemptBlocking_).
  * _ZIOTable_ now has _toCSV_, _writeCSVFileIO_ and _writeCSVFileRow_, and both modules have _parseSequence_ (they were commented out).

V1.7.x -> V2.0.0
* Scala 3 only (built with Scala 3.3 LTS; tested on 3.9 too), and Java 21. For Scala 2.13, Java 17 or Spark, use the 1.x line.
  See [Migrating from 1.x](#migrating-from-1x) for what changes in your code.
* Derivation: the parsers and renderers of case classes (_CellParser_, _CsvRenderer_, _CsvGenerator_, _HierarchicalRenderer_,
  and parquet's _ParquetCellConverter_) are derived from the case class's _Mirror_.
  This replaces the _cellParserN_, _rendererN_, _generatorN_ and _converterN_ families, removes their limits on the number of fields,
  and replaces reflection (_Reflection_ is gone).
* RFC 4180: CSV is read and written as the RFC defines it, by default (see the top of this file).
* Round trip: a table that is written can be read back: the column names written are those that the parser looks up.
* _RowConfig_ is a case class, whose regexes are derived from its delimiter and quote, and which converts to and from _CsvAttributes_
  (issues #48, #38, #32). Trimming (_RowConfig.trimmingRowConfig_) now ignores spaces before a delimiter as well as after it.
* _Content_ keeps the rows in order (it is a _Vector_; _par_ gives a parallel collection).
* New modules: _tableparser-crypto_ (encryption, from _cats_) and _tableparser-json_ (circe, replacing spray-json in _core_).
* _java.time_ instead of Joda-Time (nscala-time is no longer a dependency).
* Scala 3 idioms throughout: given/using, extension methods, enums, top-level definitions, _infix_ methods.
* _Header_ (issue #12): the leftovers of an old approach to letter names are gone, and _++_ joins extra header lines pairwise.
* Bug fixes:
  * _Table.parse(uri, enc)_ and _parseInputStream(..., enc)_ (and the cats and zio versions) ignored the encoding.
  * _parseRaw_ (and _parseFileRaw_, etc.), given a fixed header, dropped the first row of data.
  * _IOTable_ and _ZIOTable_'s _parse_ (of strings or a _Source_) failed for a parser which reads its header from the input.
  * _IOTable.toCSVRow_ and _writeCSVFileRow_ threw, rather than failing in _IO_, for a table with no header.
  * _HeadedStringTableParser_ ignored its fixed header.
  * _hexStringToBytes_ lost leading zero bytes, so about 1 in 256 encrypted rows couldn't be decrypted (also fixed in 1.7.2).
* A blank _Option\[URL]_ cell is now _None_ (it was a failure).
* Tests: about 500, now including every method of _IOTable_ and _ZIOTable_; statement coverage is 82%.

V1.7.1 -> V1.7.2
* Bug fix, back-ported from 2.0: decrypting a Hex string failed whenever the ciphertext began with a zero byte
  (_HexEncryption.hexStringToBytes_ dropped leading zero bytes), so about 1 in 256 encrypted rows couldn't be decrypted.

V1.7.0 -> V1.7.1
* Bug fixes, back-ported from 2.0:
  * When writing CSV, a String (or a column name) is quoted when it has to be, as in RFC 4180: when it contains the delimiter,
    a quote or a line break, or starts or ends with whitespace (quotes inside it are doubled).
    Up to 1.7.0, a String containing a comma or a quote was written as a broken row.
  * In multiline mode, the line break inside a quoted field is kept (it used to be dropped).
  * A column whose name matches exactly is preferred to one which differs only in case.
* New: _CsvRenderers.optionProductRenderer_ and _CsvGenerators.optionProductGenerator_, for an optional field with several columns
  (e.g. a case class): None is written as the right number of empty cells.
V1.6.0 -> V1.7.0
* The last release planned for Scala 2.13. 2.0.0 will be for Scala 3 only (see [the plan](docs/code-review-2026.md));
  the spark module stays on the 1.x line.
* Logging: the library no longer depends on logback or ships a _logback.xml_. It logs through SLF4J,
  so add the SLF4J backend of your choice (for example, logback-classic) to your own project if you want to see its logs.
* Deprecated, to leave the library in 2.0.0: the examples (_Movie_ and the crime example), _table.Main_,
  _Sequential_ (and _Sequence_, _NonSequential_, _OrderingWithZero_), and the zio module's _EvaluateZIO_.
* The zio module no longer depends on zio-http, which it didn't use.
* Dependency updates: Scala 2.13.18, cats-effect 3.7.1 (with tsec 0.6.0), zio 2.1.27, Parquet 1.18.1, Spark 4.0.4,
  and others.
* The integration tests (against full-size data files) run again, separately: `sbt +integration/test`.
V1.5.2 -> V1.6.0
* Cross-built for Scala 2.13 and Scala 3 (except for the spark module, which is 2.13 only). No API changes,
  except that a few context bounds became explicit implicit parameter lists (so that callers may pass them explicitly on Scala 3.6+)
  and `Main` (in core/table/Analysis.scala) no longer extends `App`.
* The unused dependency on flog has been removed.
V1.5.1 -> V1.5.2 (not published separately: its changes are first released in 1.6.0)
* Merged the old Indexed branch (2023) with 1.5.1.
* New _Validity_ trait, and _Table.filterValid_ to keep only the rows that are valid.
* New _Table.sample_ and _Content.sample_ to take a random sample of rows.
* New _Sequential_ module (_Sequential_, _Sequence_, _OrderingWithZero_) for tables whose rows carry a sequence number;
  it is not yet used elsewhere in the library.
* The crime example was reworked (_Crime_ and _CrimeLocation_).
V1.5.0 -> V1.5.1
* Removed debug print statements.
V1.4.1 -> V1.5.0
Implemented column-grouping of flat Parquet files (13. Grouped Case Class Mapping)
See [TableParserParquetDesign](TableParserParquetDesign.md)
V1.4.0 -> V1.4.1
* Refactoring, internal improvements such as extending csv renderers to 19-tuples.
V1.3.0 -> V1.4.0
* New Analyzer trait with deprecated methods for the old Analysis class and object.
* The old Analysis class has been renamed ColumnStatistics.
V1.2.6 -> V1.3.0
* Now supports parsing of Parquet files; _Path_ is the primary type for accessing files.
V1.2.5 -> V1.2.6
* Non-functional change to _TableParser_. The only difference is that CircleCI is not using Java 17
V1.2.3 -> V1.2.5
* Dependency updates, e.g., use Spark 4.0.1 (not 4.1.1) and Spark is now "provided."
V1.2.1 -> V1.2.3
* Minor changes, mostly in the build.sbt file.
V1.2.0 -> V1.2.1
* Added support for ZIO.
V1.1.4 -> V1.2.0
* Significant changes including the completion of the split into three packages with...
* Functioning _spark_ package.
* Now supports _Iterator_ to _Iterator_ processing.

V1.1.3 -> V1.1.4
* Split into three modules: core, cats and spark.
* Minor changes functionally speaking.

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


