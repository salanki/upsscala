// Generated by <a href="http://scalaxb.org/">scalaxb</a>.
package com.ups.ship


case class Errors(ErrorDetail: com.ups.ship.ErrorDetailType*)


case class ErrorDetailType(Severity: String,
  PrimaryErrorCode: com.ups.ship.CodeType,
  MinimumRetrySeconds: Option[String] = None,
  Location: Option[com.ups.ship.LocationType] = None,
  SubErrorCode: Seq[com.ups.ship.CodeType] = Nil,
  AdditionalInformation: Seq[com.ups.ship.AdditionalInfoType] = Nil)


case class CodeType(Code: String,
  Description: String,
  Digest: Option[String] = None)


case class AdditionalInfoType(Type: String,
  Value: Seq[com.ups.ship.AdditionalCodeDescType] = Nil)


case class AdditionalCodeDescType(Code: String,
  Description: Option[String] = None)


case class LocationType(LocationElementName: Option[String] = None,
  XPathOfElement: Option[String] = None,
  OriginalValue: Option[String] = None)
