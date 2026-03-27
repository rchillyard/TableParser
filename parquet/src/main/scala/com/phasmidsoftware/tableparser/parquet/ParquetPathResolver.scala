package com.phasmidsoftware.tableparser.parquet

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * Helper for resolving paths in single-file and dataset Parquet scenarios.
 * Centralizes logic for finding schema sources and part files.
 */
private object ParquetPathResolver {

  /**
   * Resolve the path to read schema from.
   * For directories: returns _metadata if present, else first part-*.parquet file.
   * For files: returns the file itself.
   *
   * @param path a file or directory path
   * @return the path to read schema from
   * @throws ParquetParserException if directory has no _metadata or part files
   */
  def schemaSourcePath(path: Path): Path = {
    if (Files.isDirectory(path)) {
      val metadataPath = path.resolve("_metadata")
      if (Files.exists(metadataPath)) {
        metadataPath
      } else {
        findFirstPartFile(path)
      }
    } else {
      path
    }
  }

  /**
   * Get all Parquet part files to read.
   * For directories: returns all part-*.parquet files.
   * For files: returns just the file itself.
   *
   * @param path a file or directory path
   * @return a sequence of part file paths
   */
  def allPartFiles(path: Path): Seq[Path] = {
    if (Files.isDirectory(path)) {
      Files.list(path)
              .filter(p => Files.isRegularFile(p) && p.getFileName.toString.startsWith("part-") && p.toString.endsWith(".parquet"))
              .sorted()
              .collect(java.util.stream.Collectors.toList())
              .asScala
              .toSeq
    } else {
      Seq(path)
    }
  }

  /**
   * Check if path is a dataset directory (has multiple parts or _metadata).
   *
   * @param path a file or directory path
   * @return true if path is a dataset directory
   */
  def isDataset(path: Path): Boolean = {
    Files.isDirectory(path)
  }

  /**
   * Check if a dataset has _metadata file.
   *
   * @param path a directory path
   * @return true if _metadata exists
   */
  def hasMetadata(path: Path): Boolean = {
    Files.exists(path.resolve("_metadata"))
  }

  /**
   * Find the first part-*.parquet file in a directory.
   *
   * @param path a directory path
   * @return the first part file found
   * @throws ParquetParserException if no part files exist
   */
  private def findFirstPartFile(path: Path): Path = {
    Files.list(path)
            .filter(p => Files.isRegularFile(p) && p.getFileName.toString.startsWith("part-") && p.toString.endsWith(".parquet"))
            .sorted()
            .findFirst()
            .orElseThrow(() => ParquetParserException(
              s"No _metadata or part-*.parquet files found in directory: $path",
              None
            ))
  }
}