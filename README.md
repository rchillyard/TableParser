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

