package com.phasmidsoftware.tableparser.parquet

case class YellowTaxiTrip(
                                 vendorId: Int,
                                 tpepPickupDatetime: java.time.Instant,
                                 tpepDropoffDatetime: java.time.Instant,
                                 passengerCount: Long,
                                 tripDistance: Double,
                                 ratecodeId: Long,
                                 storeAndFwdFlag: String,
                                 puLocationId: Int,
                                 doLocationId: Int,
                                 paymentType: Long,
                                 fareAmount: Double,
                                 extra: Double,
                                 mtaTax: Double,
                                 tipAmount: Double,
                                 tollsAmount: Double,
                                 improvementSurcharge: Double,
                                 totalAmount: Double,
                                 congestionSurcharge: Double,
                                 airportFee: Double
                         )
