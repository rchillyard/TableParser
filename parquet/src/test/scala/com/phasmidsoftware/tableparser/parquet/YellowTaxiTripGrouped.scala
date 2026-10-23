package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapperLower
import com.phasmidsoftware.tableparser.core.parse.{CellParsers, ColumnHelper}
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvGenerators, CsvProductGenerator, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import java.time.Instant

// ── Sub-case-classes ──────────────────────────────────────────────────────────

case class TripIdentifiers(vendorId: Option[Int], ratecodeId: Option[Long], storeAndFwdFlag: Option[String])

object TripIdentifiers extends CellParsers with CsvRenderers with CsvGenerators {
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val helper: ColumnHelper[TripIdentifiers] =
    columnHelper(
      camelToSnakeCaseColumnNameMapperLower,
      "vendorId" -> "VendorID",
      "ratecodeId" -> "RatecodeID"
    )
  implicit val converter: ParquetCellConverter[TripIdentifiers] =
    ParquetCellConverter.converter3(TripIdentifiers.apply)
  implicit val renderer: CsvRenderer[TripIdentifiers] =
    CsvRenderer.derived[TripIdentifiers]
  implicit val generator: CsvProductGenerator[TripIdentifiers] =
    CsvGenerator.derived[TripIdentifiers]
}

case class TripTiming(tpepPickupDatetime: Option[Instant], tpepDropoffDatetime: Option[Instant])

object TripTiming extends CellParsers with CsvRenderers with CsvGenerators {
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val helper: ColumnHelper[TripTiming] =
    columnHelper(camelToSnakeCaseColumnNameMapperLower)
  implicit val converter: ParquetCellConverter[TripTiming] =
    ParquetCellConverter.converter2(TripTiming.apply)
  implicit val renderer: CsvRenderer[TripTiming] =
    CsvRenderer.derived[TripTiming]
  implicit val generator: CsvProductGenerator[TripTiming] =
    CsvGenerator.derived[TripTiming]
}

case class TripGeography(puLocationId: Option[Int], doLocationId: Option[Int])

object TripGeography extends CellParsers with CsvRenderers with CsvGenerators {
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val helper: ColumnHelper[TripGeography] =
    columnHelper(
      camelToSnakeCaseColumnNameMapperLower,
      "puLocationId" -> "PULocationID",
      "doLocationId" -> "DOLocationID"
    )
  implicit val converter: ParquetCellConverter[TripGeography] =
    ParquetCellConverter.converter2(TripGeography.apply)
  implicit val renderer: CsvRenderer[TripGeography] =
    CsvRenderer.derived[TripGeography]
  implicit val generator: CsvProductGenerator[TripGeography] =
    CsvGenerator.derived[TripGeography]
}

case class FareBreakdown(
                                fareAmount: Option[Double],
                                extra: Option[Double],
                                mtaTax: Option[Double],
                                tipAmount: Option[Double],
                                tollsAmount: Option[Double],
                                improvementSurcharge: Option[Double],
                                congestionSurcharge: Option[Double],
                                airportFee: Option[Double]
                        )

object FareBreakdown extends CellParsers with CsvRenderers with CsvGenerators {
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val helper: ColumnHelper[FareBreakdown] =
    columnHelper(
      camelToSnakeCaseColumnNameMapperLower,
      "airportFee" -> "Airport_fee"
    )
  implicit val converter: ParquetCellConverter[FareBreakdown] =
    ParquetCellConverter.converter8(FareBreakdown.apply)
  implicit val renderer: CsvRenderer[FareBreakdown] =
    CsvRenderer.derived[FareBreakdown]
  implicit val generator: CsvProductGenerator[FareBreakdown] =
    CsvGenerator.derived[FareBreakdown]
}

case class TripMetrics(
                              passengerCount: Option[Long],
                              tripDistance: Option[Double],
                              paymentType: Option[Long],
                              totalAmount: Option[Double]
                      )

object TripMetrics extends CellParsers with CsvRenderers with CsvGenerators {
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val helper: ColumnHelper[TripMetrics] =
    columnHelper(camelToSnakeCaseColumnNameMapperLower)
  implicit val converter: ParquetCellConverter[TripMetrics] =
    ParquetCellConverter.converter4(TripMetrics.apply)
  implicit val renderer: CsvRenderer[TripMetrics] =
    CsvRenderer.derived[TripMetrics]
  implicit val generator: CsvProductGenerator[TripMetrics] =
    CsvGenerator.derived[TripMetrics]
}

// ── Top-level grouped case class ──────────────────────────────────────────────

case class YellowTaxiTripGrouped(
                                        ids: TripIdentifiers,
                                        timing: TripTiming,
                                        geo: TripGeography,
                                        fare: FareBreakdown,
                                        metrics: TripMetrics
                                )

object YellowTaxiTripGrouped extends CellParsers with CsvRenderers with CsvGenerators {
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val helper: ColumnHelper[YellowTaxiTripGrouped] = columnHelper()
  implicit val renderer: CsvRenderer[YellowTaxiTripGrouped] =
    CsvRenderer.derived[YellowTaxiTripGrouped]
  implicit val generator: CsvProductGenerator[YellowTaxiTripGrouped] =
    CsvGenerator.derived[YellowTaxiTripGrouped]
}