package com.phasmidsoftware.tableparser.parquet

/**
 * Exception thrown for all Parquet-specific failures, including:
 * - Schema mismatch between case class and Parquet file
 * - Schema mismatch between part files in a dataset
 * - Unsupported Parquet type with no registered mapping
 * - Corrupt or unreadable Parquet file
 *
 * @param msg   the error message.
 * @param cause an optional underlying cause.
 */
case class ParquetParserException(msg: String, cause: Option[Throwable] = None)
        extends Exception(msg, cause.orNull)