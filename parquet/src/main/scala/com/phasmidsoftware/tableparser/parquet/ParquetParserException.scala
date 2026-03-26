package com.phasmidsoftware.tableparser.parquet

case class ParquetParserException(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull)
