# TableParser Parquet Module — Design Document

**Project:** TableParser  
**Module:** `parquet`  
**Version:** 1.0 (draft)  
**Status:** Pre-implementation

---

## 1. Motivation

TableParser's core value proposition is type-safe ingestion of tabular data into typed case classes. Currently this is achieved for CSV and other text-based formats. Parquet has become the dominant format for large-scale tabular data (it is the default format in Apache Spark pipelines, and the format used by the NYC TLC taxi dataset and many other public datasets). The TLC no longer provides CSV downloads at all.

The motivation for supporting Parquet is identical to the motivation for supporting CSV in the Spark context: `spark.read.parquet(...)` produces an untyped `DataFrame`, losing all type safety. TableParser's Parquet module restores that type safety by ingesting Parquet data directly into a `Table[Row]` backed by typed case classes.

---

## 2. Scope

### In scope (this iteration)
- Reading a Parquet dataset (a directory of `.parquet` part files) or a single `.parquet` file into a `Table[Row]`
- Schema validation: Parquet file schema vs. target case class, failing fast on mismatch
- Canonical type mapping from Parquet physical and logical types to Scala types
- Column name mapping via the existing `ColumnHelper` mechanism
- `Analysis` support (works automatically since `Analysis` operates on `Table[Row]`)
- A new `ParquetParserException` for all Parquet-specific failures
- A committed binary test fixture (one month of NYC Yellow Taxi data, trimmed to ~1,000 rows)

### Deferred
- Writing a `Table[Row]` to Parquet (output direction)
- Spark module integration (Parquet-aware `Dataset[Row]` path)
- Backfilling `java.nio.file.Path` into the existing `core` module API (noted as desirable, separate PR)

---

## 3. Module Structure

A new top-level SBT module `parquet`, alongside the existing `core`, `cats`, `zio`, and `spark` modules.

```
tableparser-parquet
  src/
    main/scala/com/phasmidsoftware/tableparser/parquet/
      ParquetParser.scala         -- main entry point
      ParquetTableParser.scala    -- TableParser instance for Parquet
      ParquetRowParser.scala      -- RowParser for Parquet records
      ParquetTypeMapper.scala     -- Parquet type → Scala type mapping
      ParquetSchemaValidator.scala -- schema validation logic
      ParquetParserException.scala -- exception type
    test/scala/com/phasmidsoftware/tableparser/parquet/
      ParquetParserSpec.scala
    test/resources/
      taxi_sample.parquet         -- committed binary fixture (~1,000 rows)
```

In `build.sbt`:

```scala
lazy val parquet = project.dependsOn(core).settings(
  name := "tableparser-parquet",
  libraryDependencies ++= Seq(
    "org.apache.parquet" % "parquet-avro"   % "1.14.x",
    "org.apache.hadoop"  % "hadoop-common"  % "3.x.x" % "provided",
    "org.scalatest"     %% "scalatest"      % scalaTestVersion % Test
  )
)
```

Note: `hadoop-common` is a transitive requirement of `parquet-mr` but should be scoped `provided` since users of the `spark` module will already have it, and users of `core` alone should not be forced to pull in Hadoop.

The root aggregator in `build.sbt` is updated to include `parquet`:

```scala
lazy val root = (project in file("."))
  .aggregate(core, cats, zio, spark, parquet)
  .settings(
    name := "TableParser",
    publish / skip := true
  )
```

---

## 4. API

### 4.1 Entry Point

The primary entry point mirrors the style of `Table.parseFile` but uses `java.nio.file.Path` exclusively, avoiding all Java resource/classpath machinery.

```scala
object Table {
  // existing methods unchanged ...

  // New Parquet methods (defined in the parquet module via extension or companion):
  def parseParquet[T: TableParser](path: Path): Try[T]
}
```

For the common case of parsing directly to a `Table[Row]`, the call site looks like:

```scala
import com.phasmidsoftware.tableparser.parquet.ParquetParser._

val result: Try[Table[YellowTaxiTrip]] =
  Table.parseParquet(Path.of("data/yellow_tripdata_2024-01"))
```

Both a directory (Parquet dataset) and a single `.parquet` file are accepted. If a directory is supplied, all `.parquet` files within it are read in lexicographic order. Non-`.parquet` files in the directory (e.g. `_SUCCESS`, `_metadata`) are silently ignored.

### 4.2 ParquetTableParser

Users define a `ParquetTableParser` for their row type in an analogous way to the existing `HeadedCSVTableParser`:

```scala
object YellowTaxiTrip extends CellParsers {
  implicit val helper: ColumnHelper[YellowTaxiTrip] =
    columnHelper(camelToSnakeCaseColumnNameMapper)
  implicit val parser: CellParser[YellowTaxiTrip] = cellParser19(apply)

  implicit object TaxiTableParser extends ParquetTableParser[Table[YellowTaxiTrip]] {
    type Row = YellowTaxiTrip
    val rowParser: ParquetRowParser[YellowTaxiTrip] =
      implicitly[ParquetRowParser[YellowTaxiTrip]]
    def builder(rows: Iterator[Row]): Table[Row] =
      HeadedTable(Content(rows), /* header from Parquet metadata */)
  }
}
```

### 4.3 Path API Notes

`java.nio.file.Path` is used throughout this module. Convenience constructors are provided:

```scala
Table.parseParquet(Path.of("relative/path/to/data"))
Table.parseParquet(Path.of("/absolute/path/to/data"))
Table.parseParquet(Path.of(sys.env("DATA_DIR"), "yellow_2024"))
```

There is no `parseParquetResource` method. Resources are a Java classpath concept that does not map cleanly to Parquet datasets. Test fixtures are accessed via `Path.of(getClass.getResource(...).toURI)` where necessary, but this is confined to test code.

---

## 5. Schema Validation

### 5.1 Behaviour

On opening a Parquet file or dataset, the Parquet file schema is read from the footer metadata and validated against the target case class before any rows are parsed. If validation fails, a `ParquetParserException` is thrown and no rows are read.

Validation checks:
- Every parameter of the target case class has a corresponding column in the Parquet schema (after `ColumnHelper` name mapping is applied)
- The Parquet type of each column is compatible with the Scala type of the corresponding parameter (see Section 6)
- For datasets (multiple files), all part files have identical schemas; a mismatch between part files throws `ParquetParserException`

Columns present in the Parquet schema but not in the case class are silently ignored, consistent with the existing CSV behaviour.

### 5.2 ColumnHelper Integration

`ColumnHelper` is applied during schema validation to map case class parameter names to Parquet column names. The `camelToSnakeCaseColumnNameMapper` handles the common case (e.g. `passengerCount` → `passenger_count`). Custom mappers and aliases are supported as with CSV parsing.

### 5.3 ParquetParserException

```scala
case class ParquetParserException(msg: String, cause: Option[Throwable] = None)
  extends Exception(msg, cause.orNull)
```

This is thrown for:
- Schema mismatch between case class and Parquet file
- Schema mismatch between part files in a dataset
- Unsupported Parquet type with no registered mapping
- Corrupt or unreadable Parquet file

---

## 6. Type Mapping

### 6.1 Canonical Mapping

The following canonical mapping is defined in `ParquetTypeMapper`. Physical types are listed with their LogicalType annotations where applicable.

| Parquet Physical Type | Logical Type Annotation | Scala Type              |
|-----------------------|------------------------|-------------------------|
| `BOOLEAN`             | —                      | `Boolean`               |
| `INT32`               | —                      | `Int`                   |
| `INT32`               | `DATE`                 | `java.time.LocalDate`   |
| `INT32`               | `DECIMAL(p,s)`         | `BigDecimal`            |
| `INT64`               | —                      | `Long`                  |
| `INT64`               | `TIMESTAMP_MILLIS`     | `java.time.Instant`     |
| `INT64`               | `TIMESTAMP_MICROS`     | `java.time.Instant`     |
| `INT64`               | `DECIMAL(p,s)`         | `BigDecimal`            |
| `FLOAT`               | —                      | `Float`                 |
| `DOUBLE`              | —                      | `Double`                |
| `BYTE_ARRAY`          | `STRING` (or none)     | `String`                |
| `FIXED_LEN_BYTE_ARRAY`| `DECIMAL(p,s)`         | `BigDecimal`            |

### 6.2 Optional Fields

Parquet columns marked as `OPTIONAL` in the schema map to `Option[T]` in the case class, consistent with the existing CSV behaviour. A `REQUIRED` Parquet column mapping to an `Option[T]` parameter is permitted (the value will always be `Some`). An `OPTIONAL` Parquet column mapping to a non-`Option` parameter causes a `ParquetParserException` at schema validation time, since nulls could not be safely represented.

### 6.3 Unsupported Types

Any Parquet type not in the canonical mapping table causes a `ParquetParserException` at schema validation time with a clear message identifying the column name and type. No silent fallback to `String` is provided.

### 6.4 Extensibility

Users may register custom type mappings via an implicit `ParquetTypeMapper` instance, following the same implicit evidence pattern used elsewhere in TableParser. This allows exotic types (e.g. `MAP`, `LIST`, Parquet `GROUP` types) to be handled by application code without requiring changes to the library.

---

## 7. Row Reading

### 7.1 ParquetRowParser

Unlike `StandardRowParser`, which works with `String` inputs, `ParquetRowParser` works directly with Parquet `Group` records (the `parquet-mr` row representation). It does not go through `LineParser` or regex-based cell splitting. The path is:

```
Parquet file → RecordReader[Group] → ParquetRowParser → CellParser[Row] → Row
```

The `ParquetRowParser` reads typed values directly from each `Group` using the canonical type mapping, then passes them to the existing `CellParser` machinery. This preserves full type safety and avoids the string round-trip.

### 7.2 Row Groups and Parallelism

Parquet files are internally organised as row groups (typically 128MB each). Each row group is independent and can be read in parallel. The `ParquetTableParser` reads row groups via `parquet-mr`'s `ParquetReader` and feeds them into `Content`, which is already backed by `ParIterable`. This gives a natural parallelism opportunity that will be explored in a subsequent iteration once baseline reading is working.

### 7.3 Forgiving Mode

The existing `forgiving` flag is supported. In forgiving mode, rows that fail conversion are logged and skipped rather than causing the entire parse to fail. Schema validation failures (Section 5) always fail regardless of `forgiving` mode, since they indicate a fundamental incompatibility rather than a data quality issue.

---

## 8. Analysis Support

`Analysis` operates on `Table[Row]` and is therefore format-agnostic. No changes to `Analysis` are required. A `Table[Row]` produced from a Parquet source supports `Analysis` identically to one produced from CSV.

---

## 9. Testing

### 9.1 Test Fixture

A committed binary test fixture `taxi_sample.parquet` is included in `src/test/resources/`. This is derived from the NYC TLC Yellow Taxi January 2024 monthly Parquet file, trimmed to 1,000 rows using:

```python
import pyarrow.parquet as pq
pq.write_table(
  pq.read_table('yellow_tripdata_2024-01.parquet').slice(0, 1000),
  'taxi_sample.parquet'
)
```

This fixture provides a structurally faithful sample with real-world column names, types, and optional fields, without committing a large binary to the repository.

### 9.2 Test Case Class

A `YellowTaxiTrip` case class is defined in the test sources, modelling the NYC TLC schema:

```scala
case class YellowTaxiTrip(
  vendorId:            Int,
  tpepPickupDatetime:  java.time.Instant,
  tpepDropoffDatetime: java.time.Instant,
  passengerCount:      Option[Int],
  tripDistance:        Double,
  ratecodeId:          Option[Int],
  storeAndFwdFlag:     String,
  puLocationId:        Int,
  doLocationId:        Int,
  paymentType:         Int,
  fareAmount:          Double,
  extra:               Double,
  mtaTax:              Double,
  tipAmount:           Double,
  tollsAmount:         Double,
  improvementSurcharge:Double,
  totalAmount:         Double,
  congestionSurcharge: Option[Double],
  airportFee:          Option[Double]
)
```

### 9.3 Test Scenarios

- Happy path: parse `taxi_sample.parquet` into `Table[YellowTaxiTrip]`, verify row count and spot-check typed field values
- Schema mismatch: supply a case class with an unknown column name, expect `ParquetParserException`
- Type mismatch: supply a case class with an incompatible type for a known column, expect `ParquetParserException`
- Optional field: verify `Option[Int]` columns correctly produce `None` for null Parquet values
- Dataset: split `taxi_sample.parquet` into two part files in a directory, verify both are read and row counts sum correctly
- Analysis: run `Analysis` on a Parquet-sourced table, verify output is consistent with equivalent CSV-sourced table

---

## 10. Open Questions and Deferred Decisions

| Topic | Status | Notes |
|-------|--------|-------|
| Parquet output (write direction) | Deferred | Separate design document when in scope |
| Spark module Parquet integration | Deferred | Depends on this module being stable first |
| `Path` backfill into `core` | Deferred | Desirable; separate PR to avoid scope creep |
| Parallel row group reading | Deferred | Revisit once baseline reading is working |
| `LIST` and `MAP` Parquet types | Deferred | Supported via custom `ParquetTypeMapper`; no built-in mapping initially |
| Encryption (Parquet-MR supports encrypted Parquet) | Deferred | Out of scope for initial iteration |

---

## 11. Summary of New Types

| Type | Location | Purpose |
|------|----------|---------|
| `ParquetParserException` | `parquet` module | All Parquet-specific failures |
| `ParquetTableParser[T]` | `parquet` module | `TableParser` instance for Parquet sources |
| `ParquetRowParser[Row]` | `parquet` module | Reads typed values from Parquet `Group` records |
| `ParquetTypeMapper` | `parquet` module | Canonical Parquet→Scala type mapping; extensible |
| `ParquetSchemaValidator` | `parquet` module | Validates Parquet schema against case class at open time |

No changes are required to `core` types (`Table`, `Content`, `Header`, `Row`, `CellParser`, `ColumnHelper`, `Analysis`) for this iteration.