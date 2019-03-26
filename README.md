# TableParser
Parser of tables implemented in Scala.
Typically, the input is in the form of a 
 "CSV" (comma-separated-values) file.

Introduction
============

This library makes extensive use of type classes and other
implicit mechanisms.
Indeed, it is implemented very similarly to JSON readers.
There is a row-parser configuration mechanism which allows
the programmer to vary the regular expressions for recognizing
strings and delimiters, also to vary the quote character.

User Guide
==========

The basic structure of application code will look something like this:

        import MovieFormat._
    
        val x: Try[Table[Movie]] = for (r <- Table.parseResource("movie_metadata.csv")) yield r
     
In this example, the row type is Movie, a case class with eleven fields/parameters.
The date can be found in a local resource (relative to this class) called movie_metadata.csv.
All of the (implicit) details that characterize this particular table input are provided
in the MovieFormat object.