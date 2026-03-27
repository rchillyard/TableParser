package com.phasmidsoftware.tableparser

import com.phasmidsoftware.tableparser.core.table.{Analyzer, ColumnStatisticsProvider}
import java.nio.file.Path
import scala.reflect.ClassTag

package object parquet {

  /**
   * Implicit analyzer factory for Parquet files.
   * Allows Analysis.forParquet[Row](path) to work.
   * In scope when you `import com.phasmidsoftware.tableparser.parquet._`
   */
  implicit def parquetAnalyzerFactory[Row <: Product : ClassTag]: Path => Analyzer =
    (path: Path) => ParquetAnalyzer[Row](path)

  /**
   * Implicit ColumnStatisticsProvider for Parquet sources.
   * Allows Column.statisticsFrom(path, columnName) to work on Parquet files.
   * In scope when you `import com.phasmidsoftware.tableparser.parquet._`
   */
  implicit val parquetColumnStatisticsProvider: ColumnStatisticsProvider =
    ParquetColumnStatisticsProvider
}