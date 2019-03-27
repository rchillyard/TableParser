# TableParser
Parser of tables implemented in Scala.
Typically, the input is in the form of a 
 "CSV" (comma-separated-values) file.

Introduction
============

This library makes extensive use of type classes and other implicit mechanisms.
Indeed, it is implemented very similarly to JSON readers.
There is a row-parser configuration mechanism which allows
the programmer to vary the regular expressions for recognizing
strings and delimiters, also to vary the quote character.

User Guide
==========

The _Table_ trait expresses the result of parsing from a representation of a table.
Each row is represented by a parametric type _Row_.
Typically, this Row type is a case class with one parameter corresponding to one column in the table file.
However, some table files will have too many columns to be practical for this correspondence.
It is normal, therefore, to group the columns together logically so that each parameter itself refers to
a class which extends _Product_ (i.e. a case class or tuple).

In general, a class hierarchy will model the columns of the table.
TableParser will take care of any depth of case classes/tuples.
Currently, there is a limit of 12 parameters per case class/tuple so with a depth of _h_ classes/tuples you could
handle _12^h_ attributes altogether.

The names of the parameters of a case class do not necessarily have to be the same as the column from which the value derives.
The _ColumnHelper_ class is available to manage the mapping between parameters and columns.

The result of parsing a table file (CSV, etc.) will be a _Table[Row]_, wrapped in _Try_.
There are methods to parse most forms of text: _File, Resource, InputStream, URL, Seq[String]_, etc.

In order for _TableParser_ to know how to construct a case class (or tuple) from a set of values,
an implicit ionstance of CellParser[T] must be in scope.
This is achieved via invoking a method (from object Formats) of the following form:
where _f_ is a function which which takes _N_ parameters of types P1, P2, ... Pn respectively,
and where T is the type to be constructed:

    cellReaderN[T,P1,P2,...Pn](f)
 
Typically, the function _f_ is the _apply_ method of the case class _T_,
although you may have to explicitly refer to a particular function/method with a specific signature.

Note that _P1_, _P2_, ... _Pn_ each hava a context bound on _CellParser_ (that's to say, there is implicit
evidence of type _CellParser[P]_).
_T_ is a subtype of _Product_ and has two context bounds: _ClassTag_ and _ColumnHelper_.

Example
=======

The basic structure of application code will look something like this:

        import MovieFormat._
    
        val x: Try[Table[Movie]] = Table.parseResource("movie_metadata.csv")
     
In this example, the row type is Movie, a case class with eleven parameters.
The data can be found in a local resource (relative to this class) called movie_metadata.csv.
All of the (implicit) details that characterize this particular table input are provided
in the MovieFormat object.

The Movie class looks like this:

    case class Movie(title: String, format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Principal, genres: AttributeSet, plotKeywords: AttributeSet, imdb: String)

The MovieFormat object looks like this:

     object MovieFormat extends Formats {
     
       implicit val movieColumnHelper: ColumnHelper[Movie] = columnHelper(
         "title" -> "movie_title",
         "actor1" -> "actor_1",
         "actor2" -> "actor_2",
         "actor3" -> "actor_3",
         "plotKeywords" -> "plot_keywords",
         "imdb" -> "movie_imdb_link")
       implicit val reviewsColumnHelper: ColumnHelper[Reviews] = columnHelper(
         "imdbScore" -> "imdb_score",
         "facebookLikes" -> "movie_facebook_likes",
         "contentRating" -> "content_rating",
         "numUsersReview" -> "num_user_for_reviews",
         "numUsersVoted" -> "num_voted_users",
         "numCriticReviews" -> "num_critic_for_reviews",
         "totalFacebookLikes" -> "cast_total_facebook_likes")
       implicit val ratingColumnHelper: ColumnHelper[Rating] = columnHelper()
       implicit val formatColumnHelper: ColumnHelper[Format] = columnHelper()
       implicit val productionColumnHelper: ColumnHelper[Production] = columnHelper()
       implicit val principalColumnHelper: ColumnHelper[Principal] = columnHelper(Some("$x_$c"), "facebookLikes" -> "facebook_likes")
       implicit val multiColumnHelper: ColumnHelper[AttributeSet] = columnHelper()
       implicit val listFormat: CellParser[StringList] = cellReader(Parseable.split)
       val fRating: String => Rating = Rating.apply
       implicit val ratingFormat: CellParser[Rating] = cellReader(fRating)
       implicit val formatFormat: CellParser[Format] = cellReader4(Format.apply)
       implicit val productionFormat: CellParser[Production] = cellReader4(Production.apply)
       val fPrincipal: (String, Int) => Principal = Principal.apply
       implicit val principalFormat: CellParser[Principal] = cellReader2(fPrincipal)
       implicit val reviewsFormat: CellParser[Reviews] = cellReader7(Reviews.apply)
       val fAttributes: String => AttributeSet = AttributeSet.apply
       implicit val attributesFormat: CellParser[AttributeSet] = cellReader(fAttributes)
       implicit val movieFormat: CellParser[Movie] = cellReader11(Movie.apply)
     
      implicit object MovieConfig extends DefaultRowConfig {
        override val string: Regex = """[^\,]*""".r
        override val delimiter: Regex = """,""".r
        override val listEnclosure: String = ""
      }
     
       implicit val parser: StandardRowParser[Movie] = StandardRowParser[Movie](LineParser.apply)
     
       implicit object MovieTableParser extends TableParser[Table[Movie]] {
         type Row = Movie
     
         def hasHeader: Boolean = true
     
         override def forgiving: Boolean = true
     
         def rowParser: RowParser[Row] = implicitly[RowParser[Row]]
     
         def builder(rows: Seq[Row]): Table[Movie] = TableWithoutHeader(rows)
       }
     
     }

In this code,
_movieColumnHelper_, and the other columnHelpers, specify parameter-column mappings.

Note that _principalColumnHelper_ has an extra parameter at the start of the parameter list:
    
    Some("$x_$c")
    
which is an (optional) formatter for the purpose of prefixing a string to column names.
That's because there are several "Principal" parameters in a Movie, and each one has its own set of attributes. 
In this format parameter, $x is substituted by the prefix (the optional value passed into the lookup method)
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

A parameter can be optional.
All you have to do is declare it optional in the case class and TableParser will specify it as _Some(x)_ if valid, else _None_.