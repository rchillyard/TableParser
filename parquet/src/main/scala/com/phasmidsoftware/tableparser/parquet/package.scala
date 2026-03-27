package com.phasmidsoftware.tableparser

import com.phasmidsoftware.tableparser.core.table.{Analyzer, ColumnStatisticsProvider, DatasetAnalyzerFactory, SingleFileAnalyzerFactory}
import java.nio.file.Path

package object parquet {

  /**
   * Implicit single-file analyzer factory implementation.
   * In scope when you `import com.phasmidsoftware.tableparser.parquet._`
   */
  implicit val singleFileFactory: SingleFileAnalyzerFactory = new SingleFileAnalyzerFactory {
    def apply(path: Path): Analyzer =
      ParquetAnalyzer[Nothing](path).asInstanceOf[Analyzer]
  }

  /**
   * Implicit dataset analyzer factory implementation.
   * In scope when you `import com.phasmidsoftware.tableparser.parquet._`
   */
  implicit val datasetFactory: DatasetAnalyzerFactory = new DatasetAnalyzerFactory {
    def apply(path: Path): Analyzer =
      ParquetDatasetAnalyzer[Nothing](path).asInstanceOf[Analyzer]
  }

  /**
   * Implicit ColumnStatisticsProvider for Parquet sources.
   * Allows Column.statisticsFrom(path, columnName) to work on Parquet files and datasets.
   * In scope when you `import com.phasmidsoftware.tableparser.parquet._`
   */
  implicit val parquetColumnStatisticsProvider: ColumnStatisticsProvider =
    ParquetColumnStatisticsProvider
}