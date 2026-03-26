package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapperLower
import com.phasmidsoftware.tableparser.core.parse.{CellParsers, ColumnHelper}
import java.time.Instant

case class YellowTaxiTrip(
                                 vendorId: Option[Int],
                                 tpepPickupDatetime: Option[Instant],
                                 tpepDropoffDatetime: Option[Instant],
                                 passengerCount: Option[Long],
                                 tripDistance: Option[Double],
                                 ratecodeId: Option[Long],
                                 storeAndFwdFlag: Option[String],
                                 puLocationId: Option[Int],
                                 doLocationId: Option[Int],
                                 paymentType: Option[Long],
                                 fareAmount: Option[Double],
                                 extra: Option[Double],
                                 mtaTax: Option[Double],
                                 tipAmount: Option[Double],
                                 tollsAmount: Option[Double],
                                 improvementSurcharge: Option[Double],
                                 totalAmount: Option[Double],
                                 congestionSurcharge: Option[Double],
                                 airportFee: Option[Double]
                         )

object YellowTaxiTrip extends CellParsers {
  implicit val helper: ColumnHelper[YellowTaxiTrip] =
    columnHelper(
      camelToSnakeCaseColumnNameMapperLower,
      "vendorId" -> "VendorID",
      "ratecodeId" -> "RatecodeID",
      "puLocationId" -> "PULocationID",
      "doLocationId" -> "DOLocationID",
      "airportFee" -> "Airport_fee"
    )
}
