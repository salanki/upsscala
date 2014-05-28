package com.untzstix.ship

import scala.annotation.tailrec
import com.ups.ship._
import concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._

/* Shipping results */
case class PackageResult(trackingNo: String, labelData: Option[String])

trait MergeLabels {
  val packages: Seq[PackageResult]
  private val decoder = new sun.misc.BASE64Decoder()

  private def mergeLabels(label1: String, label2: Option[String]) = {
    import javax.imageio.ImageIO
    import java.awt.image.BufferedImage

    def rotate(image: BufferedImage) = {
      val imageGraphics = image.createGraphics()

      imageGraphics.rotate(scala.math.toRadians(90), image.getWidth() / 2, image.getHeight() / 2)
      imageGraphics.dispose()
    }
    val newImage = new BufferedImage(1400, 1650, BufferedImage.TYPE_INT_ARGB);
    val newGraphics = newImage.createGraphics()

    val image1 = ImageIO.read(new java.io.ByteArrayInputStream(decoder.decodeBuffer(label1)))
    rotate(image1)
    newGraphics.drawImage(image1, 0, 0, 1400, 800, null)

    for (l2 <- label2) {
      val image2 = ImageIO.read(new java.io.ByteArrayInputStream(decoder.decodeBuffer(l2)))
      rotate(image2)
      newGraphics.drawImage(image2, 0, 850, 1400, 800, null)
    }
    newGraphics.dispose()

    val generatedImg = new java.io.ByteArrayOutputStream()
    ImageIO.write(newImage, "png", generatedImg);

    generatedImg.toByteArray()
  }

  @tailrec private def recurseLabels(input: List[String], output: List[Array[Byte]]): List[Array[Byte]] =
    input match {
      case first :: second :: tail => recurseLabels(tail, mergeLabels(first, Some(second)) :: output)
      case first :: tail => recurseLabels(tail, mergeLabels(first, None) :: output)
      case Nil => output
    }

  /**
   * Decrease size of labels and place two labels side by side for a good fit when printing on regular paper and conservation of paper
   */
  def labelsForPrint = {
    val labelsData = for (pkg <- packages; labelData <- pkg.labelData) yield labelData
    recurseLabels(labelsData.toList, Nil)
  }

}

trait ShipResult
case class SuccessShipResult(cost: String, shipmentId: String, packages: Seq[PackageResult]) extends ShipResult with MergeLabels
case class ConfirmShipResult(cost: String, shipmentId: String, packages: Seq[PackageResult]) extends ShipResult with MergeLabels

trait FailShipResult extends ShipResult
case class AddressInvalidFailShipResult() extends FailShipResult
case class StateInvalidFailShipResult() extends FailShipResult
case class GeneralFailShipResult(code: String, description: String) extends FailShipResult

/* Shipment parts */
case class Recipient(name: String, attention: Option[String], phone: Option[String], email: Option[String], address: Seq[String], city: String, state: String, zip: String, country: String = "US", residential: Boolean = false) {
  private val shipToAddress = ShipToAddressType(AddressLine = address,
    City = city,
    StateProvinceCode = Some(state),
    PostalCode = Some(zip),
    CountryCode = country,
    ResidentialAddressIndicator = residential match {
      case true => Some("")
      case false => None
    })

  def asShipToType = ShipToType(Name = name,
    AttentionName = attention,
    Phone = phone.map(ShipPhoneType(_)),
    EMailAddress = email,
    Address = shipToAddress)
}

object ShipmentSettings {
  val shipperAddress = ShipAddressType(AddressLine = Seq("1 Test Road"),
    City = "New York",
    StateProvinceCode = Some("NY"),
    PostalCode = Some("10000"),
    CountryCode = "US")

  val shipFromAddress = shipperAddress 

  val shipper = ShipperType(Name = "My Company LLC",
    Phone = Some(ShipPhoneType("123-123-1234")),
    ShipperNumber = Some("UPS-ACCOUNT-NO-GOES-HERE"),
    EMailAddress = Some("my@email.com"),
    AttentionName = Some("comany LLC"),
    Address = shipperAddress)

  val shipFrom = ShipFromType(Name = "My Company LLC LLC",
    Phone = Some(ShipPhoneType("123-123-1234")),
    Address = shipFromAddress)

  val shipmentCharge = ShipmentChargeType(Type = "01", BillShipper = Some(BillShipperType(AccountNumber = Some("UPS-ACCOUNT-NO-GOES-HERE"))))
  val upsSecurity = UPSSecurity(UsernameToken("API USER", "API PASS"), ServiceAccessToken("API TOKEN"))

  val description = "Soft Foam LED Glowsticks"
}

/**
 * The actual shipment handling
 * This is all done via Plays WebServices API and returns everything as a future (Future) for asynchronous processing
 */
case class Shipment(recipient: Recipient, packages: Seq[Package]) {
  protected val requestType = RequestType(Seq("validate"), Some(TransactionReferenceType(CustomerContext = Some("UpsScala"))))
  protected val service = ServiceType("03", Some("Ground"))
  protected val paymentInfo = PaymentInfoType(ShipmentCharge = Seq(ShipmentSettings.shipmentCharge))
  protected val ratingOptions = RateInfoType(NegotiatedRatesIndicator = Some(""))
  protected val labelSpec = LabelSpecificationType(LabelImageFormatType("GIF", Some("GIF")))
  protected val remote = (new ShipBindings with scalaxb.Soap11PlayClients).service

  protected val shipmentType = ShipmentType(Description = Some(ShipmentSettings.description),
    Shipper = ShipmentSettings.shipper,
    ShipTo = recipient.asShipToType,
    ShipFrom = Some(ShipmentSettings.shipFrom),
    PaymentInformation = Some(paymentInfo),
    ShipmentRatingOptions = Some(ratingOptions),
    Service = service,
    Package = packages.map(_.asPackageType))

  private def parseResult(result: com.ups.ship.ShipmentResultsType) = {
    val cost = result.ShipmentCharges.get.TotalCharges.MonetaryValue
    val shipmentId = result.ShipmentIdentificationNumber.get
    val packages = result.PackageResults.map(p => PackageResult(p.TrackingNumber, p.ShippingLabel.map(_.GraphicImage)))

    SuccessShipResult(cost, shipmentId, packages)
  }
  protected def parseResponse(resp: Either[scalaxb.Soap11Fault[com.ups.ship.Errors], Any]) =
    resp match {
      case Right(ShipmentResponse(ResponseType(CodeDescriptionType("1", _), _, _), result)) => parseResult(result)
      case Right(ShipConfirmResponse(ResponseType(CodeDescriptionType("1", _), _, _), result)) => parseResult(result)
      case Left(scalaxb.Soap11Fault(_, Some(Errors(detail)), _)) => detail.PrimaryErrorCode match {
        case code if code.Code == "120802" => AddressInvalidFailShipResult()
        case code if code.Code == "120206" => StateInvalidFailShipResult()
        case code => GeneralFailShipResult(code.Code, code.Description)
      }
      case other => GeneralFailShipResult("Internal", "Unhandled response: " + other)
    }

  /**
   * Get shipment rates without actually shipping
   * Currently implemented as a shipment start without an actual accept, pretty lame
   */
  lazy val rate: Future[Either[Double, FailShipResult]] = {
    val shipmentRequest = ShipConfirmRequest(requestType, shipmentType, LabelSpecification = None)
    val resp = remote.processShipConfirm(shipmentRequest, ShipmentSettings.upsSecurity)

    resp.map(parseResponse(_) match {
      case SuccessShipResult(cost, _, _) => Left(cost.toDouble)
      case other: FailShipResult => Right(other)
    })
  }

  /**
   * Directly create a shipment
   */
  def ship(): Future[ShipResult] = {
    val shipmentRequest = ShipmentRequest(requestType, shipmentType, LabelSpecification = None)
    val resp = remote.processShipment(shipmentRequest, ShipmentSettings.upsSecurity)

    resp.map(parseResponse(_))
  }
}