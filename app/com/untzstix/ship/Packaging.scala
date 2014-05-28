package com.untzstix
package ship

import com.ups.ship._

/* Package types */
trait Package {
  val length: Double
  val width: Double
  val height: Double
  val weight: Double
  val invoiceNo: Option[Int]

  def asPackageType = {
    val dimensions = DimensionsType(UnitOfMeasurement = ShipUnitOfMeasurementType("IN", Some("Inches")),
      Length = length.toString,
      Width = width.toString,
      Height = height.toString)

    PackageType(Description = Some(ShipmentSettings.description),
      Packaging = Some(PackagingType("02", Some("customer supplied"))),
      Dimensions = Some(dimensions),
      PackageWeight = Some(PackageWeightType(ShipUnitOfMeasurementType("LBS", Some("Pounds")), weight.toString)),
      LargePackageIndicator = None,
      ReferenceNumber = Seq(ReferenceNumberType(BarCodeIndicator = None, Code = Some("IK"), invoiceNo.getOrElse("0000").toString)) )
  }
}

case class CustomPackage(length: Double, width: Double, height: Double, weight: Double, invoiceNo: Option[Int]) extends Package

