// Generated by <a href="http://scalaxb.org/">scalaxb</a>.
package com.ups.ship

import concurrent.Future

trait ShipPortType {
  def processShipment(value: com.ups.ship.ShipmentRequest, upsSecurity: com.ups.ship.UPSSecurity): Future[Either[scalaxb.Soap11Fault[com.ups.ship.Errors], com.ups.ship.ShipmentResponse]]
  def processShipConfirm(value: com.ups.ship.ShipConfirmRequest, upsSecurity: com.ups.ship.UPSSecurity): Future[Either[scalaxb.Soap11Fault[com.ups.ship.Errors], com.ups.ship.ShipConfirmResponse]]
  def processShipAccept(value: com.ups.ship.ShipAcceptRequest, upsSecurity: com.ups.ship.UPSSecurity): Future[Either[scalaxb.Soap11Fault[com.ups.ship.Errors], com.ups.ship.ShipAcceptResponse]]
}