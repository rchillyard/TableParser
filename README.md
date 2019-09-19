[![CircleCI](https://circleci.com/gh/rchillyard/TableParser.svg?style=svg)](https://circleci.com/gh/rchillyard/TableParser)

# Introduction to TableParser

A functional parser of tables implemented in Scala.
Typically, the input is in the form of a "CSV" (comma-separated-values) file.
But other formats are perfectly possible to parse.

_TableParser_ aims to make it as simple as possible to ingest a fully-typed tabular dataset.
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

For an introduction to _TableParser_ with a very simple use case, please see my blog at: https://scalaprof.blogspot.com/2019/04/new-projects.html 

# User Guide

Current version: 1.0.8.

See release notes below for history.

Parsing
=======

The _Table_ trait expresses the result of parsing from a representation of a table.
Each row is represented by a parametric type _Row_.
Typically, this _Row_ type is a case class with one parameter corresponding to one column in the table file.
However, some table files will have too many columns to be practical for this correspondence.
It is normal, therefore, to group the columns together logically so that each parameter itself refers to
a class which extends _Product_ (i.e. a case class or tuple).

In general, a class hierarchy will model the columns of the table.
_TableParser_ will take care of any depth of case classes/tuples.
Currently, there is a limit of 12 parameters per case class/tuple so with a depth of _h_ classes/tuples you could
handle _12^h_ attributes altogether.

The names of the parameters of a case class do not necessarily have to be the same as the column from which the value derives.
The _ColumnHelper_ class is available to manage the mapping between parameters and columns.

The result of parsing a table file (CSV, etc.) will be a _Table[Row]_, wrapped in _Try_.
There are object methods to parse most forms of text: _File, Resource, InputStream, URL, Seq[String]_, etc. (see _Table_ below).

The parser responsible for parsing the contents of a cell is called _CellParser[T]_ where T is the type of the value in the cell in question.
T is covariant so that if you have alternative parsers which generate different sub-classes of trait, for instance, this can be done.

In order for _TableParser_ to know how to construct a case class (or tuple) from a set of values,
an implicit instance of _CellParser[T]_ must be in scope.
This is achieved via invoking a method (from object _Parsers_) of the following form:
where _f_ is a function which which takes _N_ parameters of types _P1, P2, ... Pn_ respectively,
and where _T_ is the type to be constructed:

    cellParserN[T,P1,P2,...Pn](f)
 
Typically, the function _f_ is the _apply_ method of the case class _T_,
although you may have to explicitly refer to a particular function/method with a specific signature.
When you have created a companion object to the case class, you will simply use the method name (typically _apply_) as in
_Name.apply_ (see example below).
If you have created additional apply methods, you will need to define a function of a specific type and pass that in.
Or, more simply, do as for _ratingParser_ in the example below.

Note that _P1_, _P2_, ... _Pn_ each hava a context bound on _CellParser_ (that's to say, there is implicit
evidence of type _CellParser[P]_).
This is the mechanism which saves the programmer from having to specify explicit conversions.
_T_ is bound to be a subtype of _Product_ and has two context bounds: _ClassTag_ and _ColumnHelper_.

See section on _CellParsers_ below.

## Table

The _Table_ class has several methods for manipulation:
*  def iterator: Iterator[Row]
*  def rows: Seq[Row]
*  def maybeHeader: Option[Header]
*  def map[S](f: Row => S): Table[S]
*  def flatMap[U](f: Row => Table[U]): Table[U]
*  def unit[S](rows: Seq[S], maybeHeader: Option[Header]): Table[S]
*  def ++[U >: Row](table: Table[U]): Table[U]

It is to be expected that _join_ methods will be added later.

The following object methods are available for parsing text:
*  def parse[T: TableParser](ws: Seq[String]): Try[T]
*  def parse[T: TableParser](ws: Iterator[String]): Try[T]
*  def parse[T: TableParser](x: Source): Try[T]
*  def parse[T: TableParser](u: URI)(implicit codec: Codec): Try[T]
*  def parse[T: TableParser](u: URI, enc: String): Try[T]
*  def parse[T: TableParser](u: URL, enc: String): Try[T]
*  def parse[T: TableParser](u: URL)(implicit codec: Codec): Try[T]
*  def parse[T: TableParser](i: InputStream, enc: String): Try[T]
*  def parse[T: TableParser](i: InputStream)(implicit codec: Codec): Try[T]
*  def parse[T: TableParser](f: File)(implicit codec: Codec): Try[T]
*  def parse[T: TableParser](f: File, enc: String): Try[T]
*  def parseResource[T: TableParser](s: String, clazz: Class[_] = getClass)(implicit codec: Codec): Try[T]
*  def parseSequence[T: TableParser](wss: Seq[Seq[String]]): Try[T]

## TableParser

_TableParser_ is also the name of a trait which takes a parametric type called "Table" in its definition.
It is defined thus:

    trait TableParser[Table] {
      type Row
      def hasHeader: Boolean
      def forgiving: Boolean = false
      def rowParser: RowParser[Row]
      def builder(rows: Seq[Row]): Table
      def parse(ws: Seq[String]): Try[Table] = ...
}

The type _Row_ defines the specific row type (for example, _Movie_, in the example below).
_hasHeader_ is used to define if there is a header row in the first line of the file (or sequence of strings) to be parsed.
_forgiving_, which defaults to _false_, can be set to _true_ if you expect that some rows will not parse, but where this
will not invalidate your dataset as a whole.

In forgiving mode, any exceptions thrown in the parsing of a row are collected and then printed to _System.err_ at the conclusion of the parsing of the table.
_rowParser_ is the specific parser for the _Row_ type (see below).
_builder_ is used by the _parse_ method.
_parse_ is the main method of _TableParser_ and takes a _Seq[String]_ and yields a _Try[Table]_.

## RowParser

_RowParser_ is a trait which defines how a line of text is to be parsed as a _Row_.
_Row_ is a parametric type which, in subtypes of _RowParser_, is context-bound to _CellParser_.
A second parametric type _Input_ is defined: this will take on values of _String_ or _Seq[String]_, according to the form of input.
Typically, the _StandardRowParser_ is used, which takes as its constructor parameter a _LineParser_.

The methods of _RowParser_ are:

    def parse(w: String)(header: Header): Try[Row]

    def parseHeader(w: String): Try[Header]

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

    def parse(ws: Seq[String])(header: Header): Try[Row]

    def parseHeader(ws: Seq[String]): Try[Header]

## CellParsers

There are a number of methods which return an instance of _CellParser_ for various situations:

* def cellParserRepetition[P: CellParser : ColumnHelper](start: Int = 1): CellParser[Seq[P]]
* def cellParserSeq[P: CellParser]: CellParser[Seq[P]]
* def cellParserOption[P: CellParser]: CellParser[Option[P]]
* def cellParserOptionNonEmptyString: CellParser[Option[String]]
* def cellParser[P: CellParser, T: ClassTag](construct: P => T): CellParser[T]
* def cellParser1[P1: CellParser, T <: Product : ClassTag : ColumnHelper](construct: P1 => T, fields: Seq[String] = Nil): CellParser[T]
* etc. through cellParser12...
* def cellParser2Conditional[K: CellParser, P, T <: Product : ClassTag : ColumnHelper](construct: (K, P) => T, parsers: Map[K, CellParser[P]], fields: Seq[String] = Nil): CellParser[T]

The methods of form _cellParserN_ are the parsers which are used to parse into case classes.
Ensure that you have the correct number for N: the number of fields/parameters in the case class you are instantiating.
In some situations, the reflection code is unable to get the field names in order (for example when there are public
lazy vals).
In such a case, add the second parameter to _explicitly_ give the field names in order.
Normally, of course, you can leave this parameter unset.

There is one additional method to handle the situation where you want to vary the parser for a set of cells according
to the value in another (key) column: _cellParser2Conditional_.
In this case, you must supply a _Map_ which specifies which parser is to be used for each possible value of the key column.
If the value in that column is not one of the keys of the map, an exception will be thrown.
For an example of this, please see the example in _CellParsersSpec_ ("conditionally parse").

## Example: Movie

In this example, we parse the IMDB Movie dataset from Kaggle.
The basic structure of the application code will look something like this:

        import MovieParser._
    
        val x: Try[Table[Movie]] = Table.parseResource("movie_metadata.csv")
     
In this example, the row type is _Movie_, a case class with eleven parameters.
The data can be found in a local resource (relative to this class) called movie_metadata.csv.
All of the (implicit) details that characterize this particular table input are provided
in the _MovieParser_ object.

The _Movie_ class looks like this:

    case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Option[Principal], genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

Note that we make _actor3_ optional because some movies don't specify an "actor3".
Unlike with ordinary values such as _Int_, _Double_, we do have to add an additional implicit definition to accomplish this (see in example code below):
 
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
      def camelCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z0-9])", "_$1")
      implicit val movieColumnHelper: ColumnHelper[Movie] = columnHelper(camelCaseColumnNameMapper _,
        "title" -> "movie_title",
        "imdb" -> "movie_imdb_link")
      implicit val reviewsColumnHelper: ColumnHelper[Reviews] = columnHelper(camelCaseColumnNameMapper _,
        "facebookLikes" -> "movie_facebook_likes",
        "numUsersReview" -> "num_user_for_reviews",
        "numUsersVoted" -> "num_voted_users",
        "numCriticReviews" -> "num_critic_for_reviews",
        "totalFacebookLikes" -> "cast_total_facebook_likes")
      implicit val ratingColumnHelper: ColumnHelper[Rating] = columnHelper()
      implicit val formatColumnHelper: ColumnHelper[Format] = columnHelper(camelCaseColumnNameMapper _)
      implicit val productionColumnHelper: ColumnHelper[Production] = columnHelper(camelCaseColumnNameMapper _)
      implicit val principalColumnHelper: ColumnHelper[Principal] = columnHelper(camelCaseColumnNameMapper _, Some("$x_$c"))
      implicit val attributeSetColumnHelper: ColumnHelper[AttributeSet] = columnHelper()
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
        def hasHeader: Boolean = true
        override def forgiving: Boolean = true
        def rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

        override def builderWithHeader(rows: Seq[Row], header: Header): Table[Row] = TableWithHeader(rows, header)
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
    
In this case, some of the movies do not have a budget provided.
All you have to do is declare it optional in the case class and _TableParser_ will specify it as _Some(x)_ if valid, else _None_.

## Example: Submissions

This example has two variations on the earlier theme of the _Movies_ example:
(1) each row (a Submission) has an unknown number of _Question_ parameters;
(2) instead of reading each row from a single String, we read each row from a sequence of Strings, each corresponding to a cell.

The example comes from a report on the submissions to a Scala exam. Only one question is included in this example.

      case class Question(question_ID: String, question: String, answer: Option[String], possible_points: Int, auto_score: Option[Double], manual_score: Option[Double])
      case class Submission(username: String, last_name: String, first_name: String, questions: Seq[Question])
      object Submissions extends CellParsers {
        def baseColumnNameMapper(w: String): String = w.replaceAll("(_)", " ")
        implicit val submissionColumnHelper: ColumnHelper[Submission] = columnHelper(baseColumnNameMapper _)
        implicit val questionColumnHelper: ColumnHelper[Question] = columnHelper(baseColumnNameMapper _, Some("$c $x"))
        implicit val optionalAnswerParser: CellParser[Option[String]] = cellParserOption
        implicit val questionParser: CellParser[Question] = cellParser6(Question)
        implicit val questionsParser: CellParser[Seq[Question]] = cellParserRepetition[Question]()
        implicit val submissionParser: CellParser[Submission] = cellParser4(Submission)
        implicit val parser: StandardStringsParser[Submission] = StandardStringsParser[Submission]()
        implicit object SubmissionTableParser extends StringsTableParser[Table[Submission]] {
          type Row = Submission
          def hasHeader: Boolean = true
          override def forgiving: Boolean = false
          def rowParser: RowParser[Row, Seq[String]] = implicitly[RowParser[Row, Seq[String]]]
          def builder(rows: Seq[Row]): Table[Submission] = TableWithoutHeader(rows)
        }
      }
      val rows: Seq[Seq[String]] = Seq(
          Seq("Username", "Last Name", "First Name", "Question ID 1", "Question 1", "Answer 1", "Possible Points 1", "Auto Score 1", "Manual Score 1"),
          Seq("001234567s", "Mr.", "Nobody", "Question ID 1", "The following are all good reasons to learn Scala -- except for one.", "Scala is the only functional language available on the Java Virtual Machine", "4", "4", "")
        )

      import Submissions._
      val qty: Try[Table[Submission]] = Table.parseSequence(rows)

Note the use of _cellParserRepetition_. The parameter allows the programmer to define the start value of the sequence number for the columns.
In this case, we use the default value: 1 and so don't have to explicitly specify it.
Also, note that the instance of _ColumnHelper_ defined here has the formatter defined as "$c $x" which is in the opposite order from the Movie example.

Rendering
=========

_TableParser_ provides two mechanisms for rendering a table:
* one to a straight serialized output, for example, when rendering a table as a CSV file.
* the other to a hierarchical (i.e. tree-structured) output, such as an HTML file.

## Non-hierarchical output

For this type of output, the application programmer must provide an instance of _Writer[O]_ which is, for example a _StringBuilder_,
_BufferedOutput_, or perhaps an I/O Monad.

The non-hierarchical output does not support the same customization of renderings as does the hierarchical output.
It's intended more as a straight, quick-and-dirty output mechanism to a CSV file.

Here, for example, is an appropriate definition.

	implicit object StringBuilderWriteable extends Writable[StringBuilder] {
		override def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
		override def unit: StringBuilder = new StringBuilder
		override def delimiter: CharSequence = "|"
	}
	
The default _delimiter_ is ", ".
You can override the _newline_ and _quote_ methods too if you don't want the defaults.	

And then, folllowing this, you will write something like the following code:

    print(table.render.toString)

The _Writable_ object will take care of inserting the delimiter and quotes as appropriate.
Columns will appear in the same order as the parameters of _Row_ type (which must be either a _Product_, such as a case class, or an _Array_ or a _Seq_).
If you need to change the order of the rows, you will need to override the _writeRow_ method of _Writable_.
 
## Hierarchical output

A type class called _TreeWriter_ is the main type for rendering.
One of the instance methods of _Table[Row]_ is a method as follows:

    def render[U: TreeWriter](style: String)(implicit rr: Renderer[Row]): U
    
Providing that you have defined an implicit object of type _TreeWriter[U]_ and a _Renderer[Row]_,
then the _render_ method will produce an instance of _U_ which will be a tree containing all of the rows of this table.

What sort of type is _U_?
An XML node would be appropriate.
The specifications use a type called HTML which is provided in package _parse.render.tag_ more as an examplar rather than something definitive.

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
	
Then, we should define appropriate renderers something like as follows:

	implicit val valueRenderer: Renderer[Double] = renderer("td")
	implicit val complexRenderer: Renderer[Complex] = renderer2("tr")(Complex)

We can then write something like:

	val table = TableWithoutHeader(Seq(Complex(0, 1), Complex(-1, 0)))
	val h = table.render("table", Map("border"->"1"))
	 
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

Release Notes
=============

V1.0.7 -> V1.0.8
* build.sbt: changed scalaVersion to 2.12.9
* refactored the concept of tables with/without headers in TableParser;
* enabled program-defined headers that match Excel-style numbers or letters.

V1.0.6 -> V1.0.7
* build.sbt: changed scalaVersion to 2.12.8
* CellParser: parametric type _T_ is now covariant;
* CellParsers: added new method _cellParserOptionNonEmptyString_;
    then for each of the _cellParserN_ methods, the signature has had an defaultable _fields_ parameter to allow explicit field naming;
* Reflection: changed the message to refer to the _cellParserN_ signatures;
* README.md: fixed some issues with the doc regarding the _MovieTableParser_;
    added new features above.
    
V1.0.5 -> V1.0.6
* Added a standard implicit value of ColumnHelper for situations that don't need extra help.

V1.0.4 -> V1.0.5
* Added a convenient way of rendering a table as a non-hierarchical structure. In other words, serialization to a CSV file.

V1.0.3 -> V1.0.4
* Added the ability to add header row and header column for tables (NOTE: not finalized yet, but functional).

V1.0.2 -> V1.0.3
* Added no implicit warnings
* Created mechanism for rendering the result of parsing in a hierarchical structure.

V1.0.1 -> V1.0.2
* Added self-checking of LineParser;
* Able to parse two quote-chars together in a quotation as one quote char;
* Added enc and codec params as apporpriate to Table.parse methods.
* Added stringCellParser;
* Now, properly closes source in Table.parse methods.

V1.0.0 -> V.1.0.1
* Fixed Issue #1;
* Added parsing of Seq[Seq[String]];
* Added cellParserRepetition;
* Implemented closing of Source in _Table.parse_ methods;
* Added encoding parameters to _Table.parse_ methods.


