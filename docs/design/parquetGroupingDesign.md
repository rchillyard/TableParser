# TableParser Parquet Grouping — Design Document

**Project:** TableParser  
**Module:** `parquet`  
**Version:** TBD (post 1.4.0)  
**Status:** Design — now obsolete -- see [TableParserParquetDesign.md](TableParserParquetDesign.md)

---

## 1. Motivation

The current `ParquetRowParser` maps each field of a top-level case class directly to a Parquet column. This imposes an
arity ceiling: a case class with more than 13 fields exceeds the current `cellParser13` limit for CSV re-reading, and
exceeds the rendering arity if not extended. For wide schemas like the NYC TLC Yellow Taxi dataset (19 columns), the
flat mapping is unwieldy.

The CSV path solves this via grouped case classes: a top-level case class with N fields delegates to N sub-case-classes,
each of which reads a subset of columns from the same flat row. The grouping is defined entirely in Scala — the CSV file
itself remains flat. The position-independence comes from `ColumnHelper[T].lookup`, which resolves column names by name,
not by index.

This document designs the equivalent mechanism for Parquet.

---

## 2. How CSV Grouping Works

It is worth understanding the CSV mechanism precisely before designing the Parquet equivalent.

### 2.1 The key method: `readCellWithHeader`

In `CellParsers`, every `cellParserN` ultimately calls `readCellWithHeader`, which:

1. Calls `implicitly[ColumnHelper[T]].lookup(wo, p)` to resolve the Scala field name `p` to a column name
2. Calls `row.getIndex(columnName)` to find the column by name in the header
3. Calls `cellParser.parse(CellValue(w))` to parse the cell value

This is **position-independent**: column lookup is by name every time. The `ColumnHelper[T]` for the sub-case-class
defines the name mapping; the sub-case-class does not need to know where its columns appear in the row.

### 2.2 The `fields` parameter

Each `cellParserN` takes an optional `fields: Strings = Nil` parameter. If `Nil`, field names are obtained by reflection
from `ClassTag[T]`. If provided explicitly, they override reflection. This allows unusual cases where field names don't
match Scala identifiers.

### 2.3 The recursive delegation pattern

`cellParser12` reads `p1` from the row, then delegates to `cellParser11(construct(p1, _, ...), fs)` for the remaining
fields — passing the tail of the field names list. This is purely an implementation detail; from the user's perspective
each `cellParserN` is a flat factory.

### 2.4 What a sub-case-class needs

To participate as a grouped field in a top-level case class, a sub-case-class needs:

- Its own `implicit val helper: ColumnHelper[SubClass]` mapping its field names to column names
- Its own `implicit val parser: CellParser[SubClass]` (e.g. `cellParser3(SubClass.apply)`)

The top-level parser then treats the sub-class as just another field type with a `CellParser` instance — the fact that
it reads multiple columns is transparent.

---

## 3. The Parquet Equivalent

### 3.1 Current `ParquetRowParser` mechanism

`StandardParquetRowParser` uses reflection to enumerate the fields of the top-level case class `Row`. For each field it:

1. Resolves the Scala field name to a Parquet column name via `ColumnHelper[Row].lookup`
2. Looks up the `PrimitiveType` in the Parquet schema by column name
3. Dispatches to `ParquetCellConverter` for the resolved type

This is already name-based, not position-based — directly analogous to `readCellWithHeader`.

### 3.2 What's missing

When `StandardParquetRowParser` encounters a field of type `SubClass <: Product`, it currently tries to look up a
Parquet primitive type for that field and fails, because `SubClass` is not a primitive type and has no Parquet column.

What's needed is a `ParquetCellConverter[SubClass]` that:

1. Reads multiple fields from the same `SimpleGroup`
2. Uses `ColumnHelper[SubClass]` to resolve field names to Parquet column names
3. Dispatches to existing `ParquetCellConverter` instances for each field's primitive type

### 3.3 `converterN` factory methods

Analogous to `cellParserN`, we introduce a family of `converterN` factory methods in `ParquetCellConverter`:

```scala
def converter2[P1: ParquetCellConverter, P2: ParquetCellConverter, T <: Product : ClassTag]
              (construct: (P1, P2) => T)
              (implicit helper: ColumnHelper[T]): ParquetCellConverter[T]

def converter3[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, T <: Product : ClassTag]
              (construct: (P1, P2, P3) => T)
              (implicit helper: ColumnHelper[T]): ParquetCellConverter[T]

// ... up to converter6 or so (sub-groups are typically small)
```

Each `converterN` produces a `ParquetCellConverter[T]` that reads N fields from a `SimpleGroup` by name, using
`ColumnHelper[T]` for name resolution.

### 3.4 The `convertField` method

The existing `convertField` in `ParquetCellConverter` already takes a column name and dispatches by Parquet type. The
`converterN` implementations call it N times with the resolved column names:

```scala
def converter2[P1: ParquetCellConverter, P2: ParquetCellConverter, T <: Product : ClassTag]
              (construct: (P1, P2) => T)
              (implicit helper: ColumnHelper[T]): ParquetCellConverter[T] =
  new ParquetCellConverter[T] {
    private val Array(f1, f2) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

    def convert(group: SimpleGroup, fieldName: String): Try[T] =
      for {
        p1 <- implicitly[ParquetCellConverter[P1]].convert(group, helper.lookup(None, f1))
        p2 <- implicitly[ParquetCellConverter[P2]].convert(group, helper.lookup(None, f2))
      } yield construct(p1, p2)
  }
```

Note that `fieldName` is ignored in the `converterN` implementations — the converter reads its own fields by name. This
is consistent with how `MultiCellParser` ignores the top-level column name when parsing a sub-case-class.

### 3.5 `StandardParquetRowParser` changes

`StandardParquetRowParser` currently inspects the Parquet schema to determine types. It needs to additionally handle the
case where a field type is a `Product` sub-class with an implicit `ParquetCellConverter[T]` available:

```scala
// Current: look up Parquet primitive type for every field
// New: check for implicit ParquetCellConverter[T] first; fall back to primitive lookup
```

In practice this means the reflection loop in `StandardParquetRowParser` needs to check whether the field's Scala type
has an implicit `ParquetCellConverter` in scope. The cleanest approach is to require that the `converterN`-produced
instance is explicitly provided as an implicit in the companion object of the sub-case-class, and trust implicit
resolution to find it — exactly as `CellParser[SubClass]` is found for CSV.

### 3.6 Schema validation changes

`ParquetSchemaValidator` currently validates that every case class field has a corresponding Parquet column. With
grouping, a field of type `SubClass` will not have a corresponding Parquet column — only its constituent fields do. The
validator needs to recognise `Product` sub-types and recurse into them, validating their fields against the schema
instead.

---

## 4. Call Site

The call site for a grouped TLC case class would look like:

```scala
case class TripIdentifiers(vendorId: Option[Int], ratecodeId: Option[Long], storeAndFwdFlag: Option[String])

case class TripTiming(tpepPickupDatetime: Option[Instant], tpepDropoffDatetime: Option[Instant])

case class TripGeography(puLocationId: Option[Int], doLocationId: Option[Int])

case class FareBreakdown(fareAmount: Option[Double], extra: Option[Double], mtaTax: Option[Double],
                         tipAmount: Option[Double], tollsAmount: Option[Double],
                         improvementSurcharge: Option[Double], congestionSurcharge: Option[Double], airportFee: Option[Double])

case class TripMetrics(passengerCount: Option[Long], tripDistance: Option[Double],
                       paymentType: Option[Long], totalAmount: Option[Double])

case class YellowTaxiTripGrouped(ids: TripIdentifiers, timing: TripTiming, geo: TripGeography,
                                 fare: FareBreakdown, metrics: TripMetrics)
```

Each sub-case-class companion provides:

```scala
object TripIdentifiers extends CellParsers {
  implicit val helper: ColumnHelper[TripIdentifiers] =
    columnHelper(camelToSnakeCaseColumnNameMapperLower,
      "vendorId" -> "VendorID",
      "ratecodeId" -> "RatecodeID")
  implicit val converter: ParquetCellConverter[TripIdentifiers] =
    ParquetCellConverter.converter3(TripIdentifiers.apply)
}
```

The top-level parser then needs only:

```scala
object YellowTaxiTripGrouped extends CellParsers {
  implicit val helper: ColumnHelper[YellowTaxiTripGrouped] = columnHelper()
  implicit val converter: ParquetCellConverter[YellowTaxiTripGrouped] =
    ParquetCellConverter.converter5(YellowTaxiTripGrouped.apply)
}
```

At arity 5 on the top-level class, well within all existing ceilings.

---

## 5. Relationship to CSV Re-reading

Once grouped, the `YellowTaxiTripGrouped` case class can also be read back from the CSV output, since:

- The top-level arity is 5 — within `cellParser5`
- Each sub-class arity is at most 8 (`FareBreakdown`) — within `cellParser8`
- Each sub-class already has a `ColumnHelper` and `CellParser` from the Parquet setup

This means grouping solves both the Parquet wide-schema problem and the CSV re-read problem simultaneously.

---

## 6. Scope and Complexity

**New code:**

- `converterN` factory methods in `ParquetCellConverter` (up to arity 8 or so — mechanical boilerplate, similar to
  `cellParserN`)
- Minor changes to `StandardParquetRowParser` to detect and delegate to `ParquetCellConverter[SubClass]`
- Minor changes to `ParquetSchemaValidator` to recurse into `Product` sub-types

**Unchanged:**

- `ColumnHelper` — works as-is
- `CellParsers` / `cellParserN` — works as-is for CSV re-reading once sub-converters are defined
- `CsvRenderer` / `CsvGenerator` — works as-is for output

**Estimated complexity:** Medium. The `converterN` methods are mechanical. The tricky part is the
`StandardParquetRowParser` change — detecting at reflection time whether a field type has an implicit
`ParquetCellConverter` without causing implicit ambiguity. The typeclass approach (requiring an explicit implicit in the
companion) keeps this clean.

---

## 7. Deferred

- Truly nested Parquet schemas (Parquet `GROUP` types within the file schema itself) — not needed for TLC or similar
  flat-schema datasets
- `converterN` beyond arity 8 — sub-groups should be designed to stay small