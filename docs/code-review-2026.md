# TableParser: code review and plan for 2.0 (October 2026)

This is a review of TableParser's code as it stands at 1.6.0, the first release cross-built for Scala 2.13 and Scala 3. It isn't a review of that migration, which followed the Comparer playbook (`docs/scala3-playbook.md` in Comparer).

It has three parts:
- an assessment of the code's quality
- the dependencies and what has changed around them since TableParser was written
- a plan: what to do in 1.x, what to decide, and what 2.0.0 (Scala 3 only) should be

The figures are for the main code at the 1.6.0 release (`master` at `0dcdfd3`), in all five modules unless a module is named. The library versions are the latest on Maven Central in October 2026, and each one was checked there.

---

## 1. Summary

TableParser is **useful and well tested, but heavy**. The central idea is sound: you describe a row as a case class, and get a fully typed table, with raw rows to fall back on while you're still learning the data.

Much of the weight comes from what Scala 2 couldn't do:
- one helper per field count, up to 13 or 19 fields
- field names read by reflection
- `ClassTag`s and casts to get from run time back to compile time

Scala 3 does all of that directly, with `Mirror` and `derives`. So 2.0.0 isn't just a port. It's a chance to remove about a third of core and most of its casts.

The rest is housekeeping: example programs and a logging backend in the library jar, a parallel collection where a sequential one would be safer, and a few dependencies that are pinned, unused or in the wrong scope.

---

## 2. Strengths

- **The design is right at the top level.** `Table[Row]` with a `TableParser` type class and implicit `CellParser`s gives a typed result from very little code, and the README's examples show it.
- **The modules are split sensibly.** Core has no effect system, cats and zio add `IO` and `ZIO`, and the heavy dependencies (Parquet, Spark) are in their own modules. That split is what made the cross-build manageable.
- **The tests are substantial:** about 6,400 lines and 415 tests on 2.13 (414 on Scala 3, where spark isn't built). They passed unchanged on 2.13, 3.3 and 3.9.
- **It handles real data.** Multi-line cells, quoted delimiters, headers spread over several rows, `Option` cells, column-name mapping, and grouped Parquet columns all have tests against real files (AirBNB, crime and NYC taxi data).
- **It has very little unfinished code:** no `???`, two `var`s, and only four ignored tests.

## 3. Weaknesses

| Measure (main code: 69 files, 14,816 lines; core 47 files, 11,875 lines) | Count |
|---|---|
| `asInstanceOf` casts | 156 |
| `ClassTag` mentions | 205 |
| `implicit val`/`object` definitions | 319 |
| `TODO` / `CHECK` / `XXX` / `CONSIDER` / `FIXME` comments | 95 |
| `@deprecated` members (since 1.3.0 or 1.4.0) | 8 |
| Ignored tests | 4 |
| Statement / branch coverage (2.13, all modules) | 62.6% / 64.9% |

- **Arity families make up about a third of core.** Each helper exists once per number of fields:

  | File | Lines | Families |
  |---|---|---|
  | `render/CsvRenderers.scala` | 1,586 | `renderer1`…`renderer19`, `rendererGenerator1`…`19` |
  | `render/CsvGenerators.scala` | 851 | `generator1`…`generator19` |
  | `parse/CellParsers.scala` | 688 | `cellParser1`…`cellParser13` |
  | `render/HierarchicalRenderers.scala` | 580 | `renderer1`…`renderer13` |

  That's 3,705 lines. It also sets limits: the helpers stop at 13 fields for parsing and hierarchical rendering, and at 19 for CSV, so a wider case class has to be split into nested ones.
- **Field names come from reflection.** `util/Reflection.extractFieldNames` works out a case class's fields from its `copy$default$N` methods and declared fields. It's used in `CellParsers`, 13 times in `HierarchicalRenderers`, in `CsvGenerator` and `Table`, and in three parquet classes. It works, but it depends on how the compiler encodes case classes, which is exactly what can change between Scala versions. Scala 3's `MirroredElemLabels` gives the names at compile time.
- **Types often give way at run time.** There are 156 casts and 205 `ClassTag`s. Some are inherent in parsing text, but many exist only to get back a type that the arity families lost. Even the README's JSON example needs `.asInstanceOf[Renderable[Partnership]]`.
- **`Content` is parallel by default.** `Content.apply` wraps every table's rows in a `ParVector` (`table/Content.scala:281`), so row order isn't guaranteed unless you sort, as `Table`'s own Scaladoc notes. It also caused the one real Scala 3 bug in the migration: `Main` in `Analysis.scala` ran a parallel `foreach` during object initialization and deadlocked. For a parsing library, being sequential and deterministic by default is the safer choice, with parallelism something callers ask for.
- **Core's main sources contain things that aren't library code:**
  - the `Movie` and crime examples, in `core/examples`
  - the `Main` program in `table/Analysis.scala`
  - `table/TableWorksheet.sc`
  - `table/Sequential.scala`, whose own comment says none of it is used yet
- **Some dependencies are in the wrong scope:**
  - core depends on `logback-classic` at run time, which imposes a logging backend on every user. A library should depend only on the SLF4J API, which `scala-logging` already brings.
  - zio depends on `zio-http`, but nothing imports it.
  - zio depends on `zio-test-junit` at compile time, because `zio/util/EvaluateZIO.scala` (in main) imports `zio.test._`. So test libraries end up on every user's classpath.
- **`Table.scala` does too much.** It's 1,240 lines: the `Table` trait (which extends `Iterable`), its companion with the parse methods, `RenderableTable`, `HeadedTable`, `UnheadedTable`, `Header`, `HasKey` and `TableException`.
- **Some of the build has been left behind.**
  - The integration tests in `src/it` (six files) aren't compiled or run; the lines that would include them are commented out in `build.sbt`.
  - cats-effect is pinned at 3.6.3 ("3.7.0 appears to be missing the encryption classes that we need"), and zio at 2.1.22 ("2.1.24 is broken for some reason").
  - Until 1.6.0, parquet published two 50 MB test data files in its tests jars.

---

## 4. Dependencies

**Key:**
- **Keep:** no change needed.
- **Update:** keep it, but move to the current version.
- **Replace:** move to the alternative, in 2.0 if it changes the API.
- **Decide:** there's a real choice to make first.

| Dependency (module) | Used for | Status (October 2026) | Recommendation |
|---|---|---|---|
| `scala-parallel-collections` 1.2.0 (core) | `Content`'s `ParVector` | Maintained | **Replace** in 2.0: make `Content` sequential by default, and let callers opt in to parallel processing. |
| `scala-parser-combinators` 2.4.0 (core) | `LineParser`, `Parseable` | 2.5.0 is current. **cats-parse** `cats-parse_3` 1.1.0 is the Typelevel alternative. | **Update.** Rewriting the line parser in cats-parse isn't worth it unless it's rewritten for other reasons. |
| `spray-json` 1.3.6 (core) | `JsonTableRenderer`, `TableJsonFormat` | No release since 1.3.6. **circe** `circe-core_3` 0.14.17 and **jsoniter-scala** `jsoniter-scala-core_3` 2.41.7 are maintained, and both derive codecs with Scala 3. | **Decide.** Either replace it with circe (or jsoniter-scala) in 2.0, or move JSON support into its own small module, so that core doesn't depend on a JSON library at all. |
| `nscala-time` 3.0.0 (core) | Joda-Time `DateTime`/`LocalDate` cells, in 7 files | Joda-Time recommends `java.time`, which core already uses in 6 files. nscala-time 3.2.0 is current. | **Replace** with `java.time` in 2.0. Keep the Joda parsers in 1.x. |
| `logback-classic` (core, `Runtime`) | Logging backend | — | **Replace** now: make it `Test` only. Users choose their own backend. |
| `scala-logging` 3.9.6 (core) | Logging | Maintained | **Keep.** Flog (`flog_3` 1.0.15) is yours, but it's for debugging, not logging. |
| `tsec-cipher-jca` 0.5.0 (cats) | `Encryption` (AES for encrypted CSV columns), in one file | **0.6.0 was published in September 2026**, so tsec is still maintained. | **Update** to 0.6.0, and see whether that lets cats-effect move off 3.6.3. |
| `cats-effect` 3.6.3 (cats) | `IO`, `Resource` | 3.7.1 is current. | **Update**, once tsec allows it. |
| `zio` 2.1.22 (zio) | `ZIO` tables | 2.1.27 is current. | **Update**, and find out what broke in 2.1.24. |
| `zio-http` 3.4.0 (zio) | Nothing | — | **Remove.** |
| `zio-test-junit` (zio, compile scope) | `EvaluateZIO`, a test helper in main | — | **Replace:** move `EvaluateZIO` to the tests (deprecating it in 1.x), and make `zio-test-junit` `Test` only. |
| `parquet-column`, `parquet-hadoop` 1.15.2 (parquet) | Reading Parquet | 1.18.1 is current. | **Update.** |
| `spark-sql` 4.0.1 (spark, `provided`) | `DatasetParser` (203 lines) | Spark isn't published for Scala 3 (`spark-sql_3` doesn't exist); `spark-sql_2.13` 4.2.0 is in preview. | **Decide** (see §6, step 3). |

---

## 5. Known users

These decide what can change without notice, and what needs a migration path.

- **CSYE7200** (and `CSYE7200_Fall2026`, which is derived from it) uses `tableparser-core`, `tableparser-spark` and `tableparser-parquet` 1.5.1 (`Versions.tableParser2`). It imports `FP`, `ColumnHelper` (including `camelToSnakeCaseColumnNameMapperLower`), `CellParser`, `Table`, `TableParser` and `CsvAttributes`. Two assignments name `tableparser-core_2.13` explicitly, "Needed for FP object". CSYE7200's modules use Scala 2.12, 2.13 and 3.3.8.
- **TableParserApps** uses `tableparser-spark` 1.2.3, on Scala 2.13.17.
- **LaScala** will retire its own CSV readers (`ProductStream`, `CSV`, `TupleStream`, `Ingest`) in favour of TableParser now that it's on Scala 3 (LaScala's `docs/code-review-2026.md`, §4.4). 1.6.0 makes that possible.

## 6. Plan

1. **1.6.0**: cross-built for 2.13 and 3.3 (PR #68). Released. The follow-up branch `1_6_0_readme` fixes the README and stops parquet publishing its tests jars.
2. **1.7.0 (cross-built, no API breaks)**: housekeeping, and deprecations that point to 2.0.
   - Make `logback-classic` `Test` only, remove `zio-http`, and deprecate `EvaluateZIO`.
   - Deprecate the examples, `Main`, `TableWorksheet.sc` and `Sequential` in core, with a message saying where they'll go.
   - Update tsec to 0.6.0, and then cats-effect, zio and parquet, as far as the pins allow.
   - Either reinstate the `src/it` tests or delete them.
   - Report coverage in CI, so that 2.0 can be measured against it.
3. **Decided** (October 23, 2026):
   - **Spark** stays on the 1.x line. 2.0 is Scala 3 only, and Spark isn't published for Scala 3. `tableparser-spark` (203 lines) and TableParserApps stay on 1.x.
   - **JSON:** replace spray-json with circe in 2.0, in a new module, tableparser-json, so that core has no JSON dependency.
   - **Encryption** moves out of cats into its own module, so that the cats module depends on nothing but cats-effect.
   - **`FP`** stays in TableParser, since CSYE7200 uses it. Anything useful from LaScala's `FP` is copied in, not depended on, because LaScala is being retired. Start from the `FP` functions that CSYE7200 actually uses.
   - **Branches:** after the last 1.x release, a long-lived `1.x` branch is cut from `master` for spark and any 1.x fixes. `master` then becomes 2.x.
4. **2.0.0 (Scala 3 only)**: give TableParser the Comparer treatment.
   - **Derivation:** replace the four arity families and `Reflection` with `derives`, using `Mirror.ProductOf` for the field types and `MirroredElemLabels` for their names (playbook §6–7). This removes the 13- and 19-field limits as well as about 3,700 lines.
   - `given`/`using` throughout, with no implicit conversions or primitive givens.
   - Remove the casts that derivation makes unnecessary, and the `ClassTag`s with them.
   - Make `Content` sequential by default.
   - Move the examples and `Main` into the tests, or into TableParserApps.
   - Split `Table.scala` into the trait, the parse methods, the concrete tables and `Header`.
   - Remove everything deprecated, including the 8 members deprecated since 1.3.0 and 1.4.0.
   - Replace Joda-Time with `java.time`, and apply step 3's decisions.
   - Enums for the sealed hierarchies, the indentation-syntax rewrite, and `-Wunused:all`.
   - Raise coverage well above 62%; Comparer aims for 100%.
   - Migrate CSYE7200 (in CSYE7200, from which `CSYE7200_Fall2026` is derived) and LaScala to it.

   2.0 is built in stages, in this order, each merged into `master` by its own PR (from now on, branches and PRs are named by topic, not by number):
   1. **Groundwork** (#71): a Scala 3-only build without spark; the examples, `Main` and `EvaluateZIO` move into the tests; `Sequential` and the 1.3.0/1.4.0 deprecations are removed.
   2. **Derivation for parsing** (#72): `CellParser` derives, with field names from `Mirror`; `Reflection` goes.
   3. **Derivation for rendering** (#73): `CsvRenderer`, `CsvGenerator` and the hierarchical renderers; and a full round trip (a table that is written can be read back), with column names from `ColumnHelper`.
   4. **RFC 4180** (#74, merged into #73): reading and writing CSV as RFC 4180 defines it, by default.
   5. **Scala 3 idioms** (#76): `given`/`using`, extension methods, enums, top-level definitions, `infix def`.
   6. **Core types:** `Content` sequential by default, `Table.scala` split, casts and `ClassTag`s removed (in core, cats and zio; `Header[X]()` uses a new `FieldNames` type class instead of reflection).
   7. **Dependencies:** circe for spray-json (tableparser-json), an encryption module (tableparser-crypto), `java.time` for Joda-Time, Java 21 (from 17); and parquet's mapping of columns to case-class fields from the `Mirror` (instead of `Reflection` and `ClassTag`s).
   8. **Finish:** `-Wunused:all`, coverage (82% of statements), the README with a migration table, and the release.
      - **Deferred to its own PR after 2.0.0:** the indentation-syntax rewrite. The compiler's `-rewrite -indent` assumes consistent indentation, but some bodies of `new X { ... }` (e.g. in `CellParsers`, `Parseable`, `TableParser`, `HierarchicalRenderers`) are not indented beyond the `new`, so removing their braces moves them out of their classes. Fix that indentation first (a layout-only change), then rewrite each module, checking that it still compiles and its tests pass.
      - **TODO: resolve the five open GitHub issues** (fix each, or close it with a note on why it no longer applies):
        - [#44](https://github.com/rchillyard/TableParser/issues/44) Incompatible with Scala Parser Combinators version 2. **Closed** (2.x builds with scala-parser-combinators 2.5.0).
        - [#12](https://github.com/rchillyard/TableParser/issues/12) Header-related code is inconsistent and confusing. **Done on `2_0_0_finish`**: the leftovers of an abandoned approach to letter names are gone (`multiply`, `prepend`, `alphabet`), `++` joins the extra header lines pairwise (it made their cartesian product), and `Header`'s Scaladoc lists the ways to make one. Excel-style headers (`Header(letters, length)`) already worked, and are now tested past ZZ. Close it when the PR is merged.
        - [#48](https://github.com/rchillyard/TableParser/issues/48) Need method for default RowConfig, [#38](https://github.com/rchillyard/TableParser/issues/38) CSV parsing should use CsvAttributes, and [#32](https://github.com/rchillyard/TableParser/issues/32) The way parsers are configured does not use lens pattern. **Done on `2_0_0_finish`** (Robin chose a case class):
          - `RowConfig` is a case class of characters (`delimiter`, `quote`, `listSep`, `listEnclosure`, `trim`, and an optional explicit `string` regex).
          - The regexes are derived from them, so changing the delimiter changes the string regex too (#48).
          - `withDelimiter` etc. are copy methods (#32; `TableParser`'s `setX` methods already worked this way).
          - `RowConfig.csvAttributes` and `CsvAttributes.rowConfig` convert between reading and writing (#38).
          - `DefaultRowConfig` is gone: `given RowConfig = RowConfig(delimiter = '\t')` replaces `extends DefaultRowConfig { override val ... }`.
          - Trimming (`trim = true`, as in 1.x) now drops spaces before a delimiter as well as after it.
          
          Close the three when the PR is merged.

   (The earliest PRs used an older numbering: "stage 3" was derivation for parsing, "stage 4" derivation for rendering, and "stage 2" the idioms.)
5. **2.1.0 (possible)**: features which build on 2.0.
   - **TODO: RFC 7111** ([URI fragment identifiers for `text/csv`](https://www.rfc-editor.org/rfc/rfc7111)), the companion to RFC 4180. A fragment selects part of a CSV file: rows (`#row=5-7`, `#row=10-*`), columns (`#col=2-4`), cells (`#cell=4,1-6,2`, a rectangle), or several at once (`#row=5-7;row=10`). Rows and columns are numbered from 1. Check in the RFC whether the header line counts as row 1.
     1. A `CsvFragment` type (selections of rows, columns or cells, as ranges), with a parser for the fragment string (parser combinators, as `LineParser` uses).
     2. `table.select(fragment)`: rows map onto the existing `select`/`slice`; columns onto the header.
     3. `Table.parse(uri)` (and `IOTable`/`ZIOTable`) applies the URI's fragment, which it currently ignores. A column selection can't always make a `T`, so: rows for any table, columns and cells for raw tables only (or applied to the raw rows before they're converted).
     4. An `Rfc7111Spec` beside `Rfc4180Spec`, working through the RFC's examples (which would back a badge, as for RFC 4180).
   - **TODO: streaming in the cats and zio modules.** Today `IOTable` and `ZIOTable` only wrap core's `Try` methods (after 2.0.1's fixes, they at least do so lazily and close their sources), and they read the whole table into memory, as core does. What an effect library offers for CSV is reading a large file row by row, in constant memory, with the file closed reliably:
     1. cats: `IOTable.stream[Row]: fs2.Stream[IO, Row]` (adds an fs2 dependency to _tableparser-cats_), with the source as a `Resource`.
     2. zio: `ZIOTable.stream[Row]: ZStream[Any, Throwable, Row]` (ZStream is part of ZIO), with the source scoped.
     3. Both: the header from the first row(s), then each line parsed by the `RowParser`; in forgiving mode, bad rows are logged and skipped, otherwise the stream fails. Multiline rows (quoted line breaks) must be joined as `Joinable.incomplete` does in core.
     4. Writing: a `Pipe`/`ZSink` that writes rows as CSV (with the header first).
     5. Consider a typed error channel for zio (e.g. `ZStream[Any, ParserException, Row]`) rather than `Throwable`.
     6. Whether the two modules could share one implementation over cats-effect's `Sync[F]` (zio-interop-cats gives `Sync[Task]`): that would make zio users depend on cats, so probably not.
     No known user (§5) uses either module yet, so their APIs can change freely.
6. **Each release**: update the README's dependency example and release notes, and check the docs on javadoc.io.

## Appendix: how the figures were measured

All were measured in `*/src/main/scala` at the 1.6.0 release, unless noted:
- **Casts:** `grep -rhoE 'asInstanceOf'`
- **`ClassTag`s:** `grep -rhoE 'ClassTag'`
- **Implicit definitions:** `grep -rhoE 'implicit (lazy )?(val|object)'`
- **Comment markers:** `grep -rhoE '\b(TODO|CHECK|XXX|CONSIDER|FIXME)\b'`
- **Ignored tests:** `ignore should` in `*/src/test`
- **Arity families:** `grep -hoE 'def [a-zA-Z]+[0-9]+\b'` in the four files named in §3
- **Coverage:** `sbt coverage test coverageAggregate` on 2.13.17, during the migration
- **Versions:** the `<release>` in each artifact's `maven-metadata.xml` on Maven Central, except circe, where that is a milestone (0.15.0-M1) and the latest stable release is given instead
