package controllers

import java.io.File
import java.io.FileOutputStream
import sun.misc.BASE64Decoder

import com.typesafe.plugin._
import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.cache.Cache
import play.api.libs.concurrent.Execution.Implicits._

import com.untzstix.ship._
import views.html.Shipping._
import Implicits._

object Shipping extends Controller {
  private val labelDir = "public/labels/"
  private val decoder = new BASE64Decoder()

  private def printToFile(f: java.io.File)(op: java.io.FileOutputStream => Unit) {
    val p = new FileOutputStream(f)
    try { op(p) } finally { p.close() }
  }

  private def handleError(err: FailShipResult) = BadRequest(shipError(err match {
    case _: StateInvalidFailShipResult => "Invalid state code. Needs to be two letter ie: NY,NJ,CA"
    case _: AddressInvalidFailShipResult => "Invalid address"
    case other => other.toString
  }))
  
  def rateShip(countStr: String, name: String, attention: Option[String], phone: Option[String], email: Option[String], address1: String, address2: Option[String], city: String, state: String, zip: String, residential: Boolean, orderNo: Int, action: String, accountName: Option[String]) = Action {
    val count = countStr.delimitedToInt

    if (count < 1) BadRequest(shipError("Stick count is zero. Plese make sure that Stick Count is correctly set in the Opportunity."))
    else {
      val recipient = Recipient(name = name, attention = attention, phone = phone, email = email, address = Seq(address1, address2.getOrElse("")), city = city, state = state, zip = zip, residential = residential)

      val boxes: Seq[Package] = Nil // This is where you put your packages
      val shipment = Shipment(recipient, boxes)


      
      Async {
        action match {
          case "rate" => for (response <- shipment.rate) yield response match {
            case Left(cost) => Ok("nothing") // RENDER A VIEW
            case Right(err) => handleError(err)
          }
          case "ship" => for (response <- shipment.ship()) yield response match {
            case SuccessShipResult(cost, shipmentId, packages) => {
              /* Write the label data */
              // Old way of writing to a file:  packages.foreach(p => printToFile(new File(labelDir + p.trackingNo + ".gif")) { w => p.labelData.map(decoder.decodeBuffer(_)) foreach (w.write) })
              for (p <- packages) Cache.set("labeldata." + p.trackingNo, p.labelData.map(decoder.decodeBuffer(_)).get, 43200)
              Cache.set("printlabels." + shipmentId, response.asInstanceOf[SuccessShipResult].labelsForPrint, 43200) /* Easiest way to allow optional printing of labels in a nice format*/
              Ok("nothing") // RENDER A VIEW
            }
            case err: FailShipResult => handleError(err)
          }
        }
      }
    }
  }


  def label(trackingNo: String) = Action {
    Cache.getAs[Array[Byte]]("labeldata." + trackingNo) match {
      case None => BadRequest("Failed to read label from cache")
      case Some(data) => Ok(data).as("image/gif")
    }
  }

}