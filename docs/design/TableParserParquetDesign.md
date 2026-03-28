# TableParser Parquet Module — Design Document

**Project:** TableParser  
**Module:** `parquet`  
**Version:** 1.4.0  
**Status:** Implemented

---

## 1. Motivation

TableParser's core value proposition is type-safe ingestion of tabular data into typed case classes.
Currently this is achieved for CSV and other text-based formats.
Parquet has become the dominant format for large-scale tabular data (it is the default format in Apache Spark pipelines,
and the format used by the NYC TLC taxi dataset and many other public datasets).
The TLC no longer provides CSV downloads at all.

The motivation for supporting Parquet is identical to the motivation for supporting CSV in the Spark context:
`spark.read.parquet(...)` produces an untyped `DataFrame`, losing all type safety.
TableParser's Parquet module restores that type safety by ingesting Parquet data directly into a `Table[Row]` backed by typed case classes.

---

## 2. Scope

### Implemented in this iteration
- Reading a single `.parquet` file into a `Table[Row]`
- Schema validation: Parquet file schema vs. target case class, failing fast on mismatch
- Canonical type mapping from Parquet physical and logical types to Scala types
- Column name mapping via the existing `ColumnHelper` mechanism, including a new `camelToSnakeCaseColumnNameMapperLower` mapper added to `core`
- A new `ParquetParserException` for all Parquet-specific failures
- A committed binary test fixture (NYC Yellow Taxi January 2024 data, trimmed to 1,000 rows)
- A `TableBuilder` trait extracted from `TableParser` in `core` to cleanly support non-string source types
- CSV rendering of Parquet-sourced tables: `CsvRenderer` and `CsvGenerator` extended to arity 19, with new bare-type and
  `Option` instances for `Float`, `Short`, `Byte`, `Instant`, `Temporal`, and `Option[Long]`
- `YellowTaxiTrip` companion object wired with `renderer19` and `generator19`, enabling direct output of
  Parquet-ingested taxi data to CSV via `CsvTableFileRenderer` / `CsvTableStringRenderer`

### Deferred
- Writing a `Table[Row]` to Parquet (output direction)
- Direct Spark module integration (Parquet-aware `Dataset[Row]` path). 
  Many tables can already be read from Parquet and then converted into Spark `Dataset`.
- Parallel row group reading
- `BigDecimal` scale handling (currently hardcoded to 0; see Section 7.3)
- Grouped case class mapping for flat Parquet schemas: mapping multiple flat Parquet columns onto nested case classes (
  analogous to the IMDB movie database grouping pattern in `core`), avoiding the arity ceiling for wide schemas

---

## 3. Module Structure

A new top-level SBT module `parquet`, alongside the existing `core`, `cats`, `zio`, and `spark` modules.

```
tableparser-parquet
  src/
    main/scala/com/phasmidsoftware/tableparser/parquet/
      ParquetParser.scala          -- main entry point
      ParquetTableParser.scala     -- TableBuilder instance for Parquet
      ParquetRowParser.scala       -- row parser for Parquet SimpleGroup records
      ParquetCellConverter.scala   -- type class for direct typed value extraction
      ParquetTypeMapper.scala      -- Parquet type → Scala type mapping
      ParquetSchemaValidator.scala -- schema validation logic
      ParquetParserException.scala -- exception type
      YellowTaxiTrip.scala         -- example case class for NYC TLC Yellow Taxi data
    test/scala/com/phasmidsoftware/tableparser/parquet/
      ParquetParserSpec.scala
    test/resources/
      taxi_sample.parquet          -- committed binary fixture (1,000 rows)
```

In `build.sbt`:

```scala
lazy val parquet = project.dependsOn(core).settings(
  name := "tableparser-parquet",
  libraryDependencies ++= Seq(
    "org.apache.parquet" % "parquet-column"             % "1.15.2",
    "org.apache.parquet" % "parquet-hadoop"             % "1.15.2",
    "org.apache.hadoop"  % "hadoop-common"              % "3.4.1" % "provided",
    "org.apache.hadoop"  % "hadoop-mapreduce-client-core" % "3.4.1" % Test,
    "org.scalatest"     %% "scalatest"                  % scalaTestVersion % Test
  )
)
```

Note: `parquet-avro` is NOT used. The module works directly with `parquet-mr`'s native `SimpleGroup` / `GroupReadSupport` API, avoiding the Avro object model entirely. `hadoop-mapreduce-client-core` is required at test runtime but not in production (where Hadoop is expected on the classpath).

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

## 4. Core Refactoring: TableBuilder

A prerequisite to implementing the `parquet` module cleanly was extracting a `TableBuilder` trait from `TableParser` in `core`. The existing `TableParser` trait is built around an `Iterator[String]` input pipeline which has no meaning for Parquet sources. Rather than force-fitting `ParquetTableParser` into that hierarchy (with stub implementations of `rowParser`, `parse(Iterator)` etc.), a thin base trait was extracted:

```scala
trait TableBuilder[Table] {
  type Row
  protected def builder(rows: Iterator[Row], header: Header): Table
  protected val forgiving: Boolean = false
  protected val predicate: Try[Row] => Boolean = includeAll
}
```

`TableParser` now extends `TableBuilder`, so all existing code is unchanged. `ParquetTableParser` extends `TableBuilder` directly, gaining `builder`, `forgiving`, and `predicate` without inheriting the string-parsing machinery.

---

## 5. API

### 5.1 Entry Point

```scala
object ParquetParser {
  def parse[Row <: Product : ClassTag](
    path: Path
  )(implicit helper: ColumnHelper[Row]): Try[Table[Row]]
}
```

The call site looks like:

```scala
import com.phasmidsoftware.tableparser.parquet.ParquetParser

implicit val helper: ColumnHelper[YellowTaxiTrip] =
  columnHelper(camelToSnakeCaseColumnNameMapperLower, ...)

val result: Try[Table[YellowTaxiTrip]] =
  ParquetParser.parse[YellowTaxiTrip](Path.of("data/taxi_sample.parquet"))
```

`java.nio.file.Path` is used exclusively. There is no `parseParquetResource` method — test fixtures are accessed via `Path.of(getClass.getResource(...).toURI)`.

### 5.2 CSV Output

A Parquet-sourced `Table[T]` is rendered to CSV identically to any other `Table[T]`, since the render pipeline is
source-agnostic. Given appropriate implicits in the companion object:

```scala
val table: Try[Table[YellowTaxiTrip]] =
  ParquetParser.parse[YellowTaxiTrip](path)

// Render to file
table.flatMap(CsvTableFileRenderer[YellowTaxiTrip](outputPath).render(_))

// Render to string
table.map(CsvTableStringRenderer[YellowTaxiTrip]().render(_))
```

### 5.3 ColumnHelper and Name Mapping

A new mapper was added to `ColumnHelper` in `core`:

```scala
val camelToSnakeCaseColumnNameMapperLower: String => String =
  camelToSnakeCaseColumnNameMapper andThen (_.toLowerCase)
```

This correctly maps `passengerCount` → `passenger_count`. The existing `camelToSnakeCaseColumnNameMapper` is unchanged (it does not lowercase).

Real-world Parquet schemas often have inconsistent casing (e.g. TLC uses `VendorID`, `RatecodeID`, `Airport_fee`). These are handled via aliases in `ColumnHelper`:

```scala
implicit val helper: ColumnHelper[YellowTaxiTrip] =
  columnHelper(
    camelToSnakeCaseColumnNameMapperLower,
    "vendorId"    -> "VendorID",
    "ratecodeId"  -> "RatecodeID",
    "puLocationId" -> "PULocationID",
    "doLocationId" -> "DOLocationID",
    "airportFee"  -> "Airport_fee"
  )
```

---

## 6. Schema Validation

### 6.1 Behaviour

On opening a Parquet file, the schema is read from the footer metadata and validated against the target case class before any rows are parsed. If validation fails, a `ParquetParserException` is thrown and no rows are read.

Validation checks:
- Every parameter of the target case class has a corresponding column in the Parquet schema (after `ColumnHelper` name mapping is applied)
- Every such column has a supported Parquet type (via `ParquetTypeMapper`)
- Any `OPTIONAL` Parquet column must map to an `Option[T]` parameter — mapping to a non-`Option` parameter throws `ParquetParserException`

Columns present in the Parquet schema but absent from the case class are silently ignored, consistent with existing CSV behaviour.

Note: In practice, real-world Parquet datasets (including all TLC Yellow Taxi 2024 data) may mark every column as `OPTIONAL`. Users should declare all fields as `Option[T]` unless they have specific knowledge that a column is `REQUIRED`.

### 6.2 ParquetParserException

```scala
case class ParquetParserException(msg: String, cause: Option[Throwable] = None)
  extends Exception(msg, cause.orNull)
```

---

## 7. Type Mapping

### 7.1 Canonical Mapping

Defined in `ParquetTypeMapper`:

| Parquet Physical Type   | Logical Type Annotation  | Scala Type                  |
|-------------------------|--------------------------|-----------------------------|
| `BOOLEAN`               | —                        | `Boolean`                   |
| `INT32`                 | —                        | `Int`                       |
| `INT32`                 | `DATE`                   | `java.time.LocalDate`       |
| `INT32`                 | `DECIMAL(p,s)`           | `BigDecimal`                |
| `INT64`                 | —                        | `Long`                      |
| `INT64`                 | `TIMESTAMP_MILLIS/MICROS`| `java.time.Instant`         |
| `INT64`                 | `DECIMAL(p,s)`           | `BigDecimal`                |
| `FLOAT`                 | —                        | `Float`                     |
| `DOUBLE`                | —                        | `Double`                    |
| `BINARY`                | `STRING` (or none)       | `String`                    |
| `FIXED_LEN_BYTE_ARRAY`  | `DECIMAL(p,s)`           | `BigDecimal`                |

Note: `BINARY` without a logical type annotation is treated as `String`. The `large_string` type reported by PyArrow is an Arrow concept; at the Parquet level it is `BINARY` with `StringLogicalTypeAnnotation`.

### 7.2 Optional Fields and Type Erasure

A key implementation detail: for `Option[T]` fields, generic type reflection (`getActualTypeArguments`) returns `java.lang.Object` at runtime due to JVM type erasure. The inner type cannot be recovered this way.

The solution is to use the Parquet schema itself as the authoritative source of type information. `convertOption` takes the `PrimitiveType` from the schema rather than the field's generic type:

```scala
def convertOption(
  group:       SimpleGroup,
  columnName:  String,
  parquetType: PrimitiveType
): Try[Any] =
  if (group.getFieldRepetitionCount(columnName) == 0) Success(None)
  else ParquetTypeMapper.mapType(parquetType) match {
    case Left(ex)     => Failure(ex)
    case Right(clazz) => convertByClass(group, columnName, clazz).map(Some(_))
  }
```

`convertByClass` matches against both primitive and boxed types (e.g. `classOf[Int]` and `classOf[java.lang.Integer]`) to handle JVM boxing correctly.

### 7.3 BigDecimal Scale Limitation

`BigDecimalConverter` currently hardcodes scale to `0`.
The correct scale is available in `DecimalLogicalTypeAnnotation` but passing `PrimitiveType` through to all converters is deferred. 
This converter will produce incorrect results for Parquet decimals with non-zero scale.

---

## 8. Analysis Support

`Analysis` in `core` operates on `RawTable` (`Table[RawRow]`), not on typed `Table[Row]`. A `Table[YellowTaxiTrip]` produced from a Parquet source therefore does not support `Analysis` directly. This was not apparent from the design document and is noted here as a correction.

Supporting `Analysis` on Parquet-sourced tables is deferred. Options include a separate raw Parquet read path or a dedicated column statistics mechanism in the `parquet` module.

---

## 9. Row Reading

### 9.1 ParquetCellConverter

A new type class `ParquetCellConverter[T]` was introduced to extract typed values directly from a `SimpleGroup`, bypassing the `String`-based `CellParser` machinery of `core` entirely:

```scala
trait ParquetCellConverter[T] {
  def convert(group: SimpleGroup, fieldName: String): Try[T]
}
```

Instances are provided for `Boolean`, `Int`, `Long`, `Float`, `Double`, `String`, `Instant`, `LocalDate`, and `BigDecimal`. The companion object provides `convertField`, `convertOption`, and `convertByClass` dispatch methods.

### 9.2 Read Path

```
java.nio.file.Path
  → HadoopPath
  → ParquetFileReader.open (schema from footer)
  → ParquetSchemaValidator.validate
  → StandardParquetRowParser.apply (converters built once via reflection)
  → ParquetReader[Group] / GroupReadSupport
  → Iterator.unfold → Iterator[Row]
  → builder → HeadedTable[Row] (forces materialisation into Content)
```

Reflection cost (field inspection, converter construction) is paid once at parser construction time, not per row.

### 9.3 Resource Management

`ParquetReader` is `AutoCloseable`. The `Iterator.unfold` approach produces a lazy iterator, but `builder` immediately materialises it into `Content` (via `HeadedTable`), which exhausts the reader. If `builder` is overridden to return a lazy structure, resource management must be revisited.

### 9.4 Dataset Support (Deferred)

`ParquetReader` with `GroupReadSupport` does not handle directories natively — it requires a single file path. Directory (dataset) support is deferred and requires enumerating part files and reading them sequentially or in parallel.

---

## 10. Testing

### 10.1 Test Fixture

`taxi_sample.parquet` — 1,000 rows from NYC TLC Yellow Taxi January 2024, generated via:

```python
import pyarrow.parquet as pq
pq.write_table(
  pq.read_table('yellow_tripdata_2024-01.parquet').slice(0, 1000),
  'taxi_sample.parquet'
)
```

### 10.2 Test Case Class

All 19 columns in the 2024 TLC Yellow Taxi schema are `OPTIONAL`. The case class reflects this:

```scala
case class YellowTaxiTrip(
  vendorId:             Option[Int],
  tpepPickupDatetime:   Option[Instant],
  tpepDropoffDatetime:  Option[Instant],
  passengerCount:       Option[Long],
  tripDistance:         Option[Double],
  ratecodeId:           Option[Long],
  storeAndFwdFlag:      Option[String],
  puLocationId:         Option[Int],
  doLocationId:         Option[Int],
  paymentType:          Option[Long],
  fareAmount:           Option[Double],
  extra:                Option[Double],
  mtaTax:               Option[Double],
  tipAmount:            Option[Double],
  tollsAmount:          Option[Double],
  improvementSurcharge: Option[Double],
  totalAmount:          Option[Double],
  congestionSurcharge:  Option[Double],
  airportFee:           Option[Double]
)
```

The companion object provides `CsvRenderer` and `CsvGenerator` instances via `renderer19` and `generator19`, enabling
direct CSV output of Parquet-ingested data.

### 10.3 Test Scenarios

- Happy path: parse `taxi_sample.parquet` into `Table[YellowTaxiTrip]`, verify 1,000 rows and spot-check typed field values
- Header: verify 19 columns in the header
- CSV output: render `Table[YellowTaxiTrip]` to CSV and verify header row and data rows
- Schema mismatch: supply a case class with an unknown column name, expect `ParquetParserException`
- OPTIONAL/non-Option mismatch: pending (all columns happen to be OPTIONAL in the fixture)
- Dataset (multi-file): pending — deferred until dataset support is implemented
- Analysis: pending — deferred (see Section 8)

---

## 11. Open Questions and Deferred Decisions

| Topic                                                  | Status   | Notes                                                                     |
|--------------------------------------------------------|----------|---------------------------------------------------------------------------|
| Parquet dataset (directory) support                    | Deferred | `ParquetReader` needs directory handling                                  |
| Parquet output (write direction)                       | Deferred | Separate design document when in scope                                    |
| Spark module Parquet integration                       | Deferred | Depends on this module being stable first                                 |
| Parallel row group reading                             | Deferred | Revisit once baseline reading is working                                  |
| `LIST` and `MAP` Parquet types                         | Deferred | No built-in mapping; custom `ParquetTypeMapper` possible                  |
| `BigDecimal` scale from `DecimalLogicalTypeAnnotation` | Deferred | Currently hardcoded to 0                                                  |
| `Analysis` on Parquet-sourced tables                   | Deferred | Requires raw read path or separate statistics mechanism                   |
| Grouped case class mapping for flat schemas            | Deferred | Analogous to IMDB grouping pattern; avoids arity ceiling for wide schemas |
| Encryption                                             | Deferred | Out of scope for initial iteration                                        |

---

## 12. Summary of New and Modified Types

### New Types in `parquet` module

| Type                                                      | Purpose                                                                          |
|-----------------------------------------------------------|----------------------------------------------------------------------------------|
| `ParquetParserException`                                  | All Parquet-specific failures                                                    |
| `ParquetTableParser[R]`                                   | `TableBuilder` instance for Parquet sources                                      |
| `ParquetRowParser[Row]` / `StandardParquetRowParser[Row]` | Converts `SimpleGroup` records to typed `Row`                                    |
| `ParquetCellConverter[T]`                                 | Type class for direct typed value extraction from `SimpleGroup`                  |
| `ParquetTypeMapper`                                       | Canonical Parquet→Scala type mapping                                             |
| `ParquetSchemaValidator`                                  | Validates Parquet schema against case class at open time                         |
| `ParquetParser`                                           | Entry point: `parse[Row](path)`                                                  |
| `YellowTaxiTrip`                                          | Example case class for NYC TLC Yellow Taxi data with CSV render/generate support |

### Changes to `core`

| Item                                    | Change                                                                                                                                                                                                                                                          |
|-----------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `TableBuilder[Table]` trait             | Added; thin base trait extracted from `TableParser`                                                                                                                                                                                                             |
| `TableParser[Table]`                    | Now extends `TableBuilder[Table]`; existing code unchanged                                                                                                                                                                                                      |
| `camelToSnakeCaseColumnNameMapperLower` | Added to `ColumnHelper`                                                                                                                                                                                                                                         |
| `CsvRenderers` / `CsvGenerators` traits | Extended to arity 19 (previously 13)                                                                                                                                                                                                                            |
| `CsvRenderers` companion                | Added `CsvRendererFloat`, `CsvRendererShort`, `CsvRendererByte`, `CsvRendererInstant`, `CsvRendererTemporal`; added `rendererOptionFloat`, `rendererOptionShort`, `rendererOptionByte`, `rendererOptionInstant`, `rendererOptionTemporal`, `rendererOptionLong` |
| `CsvGenerators` companion               | Added `CsvGeneratorFloat`, `CsvGeneratorShort`, `CsvGeneratorByte`, `CsvGeneratorInstant`, `CsvGeneratorTemporal`                                                                                                                                               |
| `CsvGenerator` companion                | Added `floatGenerator`, `shortGenerator`, `byteGenerator`, `instantGenerator`, `temporalGenerator`                                                                                                                                                              |