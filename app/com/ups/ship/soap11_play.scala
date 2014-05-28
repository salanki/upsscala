package scalaxb

import play.api.libs.ws.WS
import concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._

trait Soap11PlayClients {
  lazy val soapClient: Soap11Client = new Soap11Client {}
  def baseAddress: java.net.URI

  trait Soap11Client {
    import soapenvelope11._
    val SOAP_ENVELOPE11_URI = "http://schemas.xmlsoap.org/soap/envelope/"

    def soapRequest(in: Option[Envelope], scope: scala.xml.NamespaceBinding,
                    address: java.net.URI, webMethod: String, action: Option[java.net.URI]): Future[Envelope] = {
      val merged = scalaxb.toScope(((Some("soap11") -> SOAP_ENVELOPE11_URI) ::
        scalaxb.fromScope(scope)).distinct: _*)
      val r = in map  { scalaxb.toXML(_, Some(SOAP_ENVELOPE11_URI), Some("Envelope"), merged) match {
        case elem: scala.xml.Elem => elem
        case x => sys.error("unexpected non-elem: " + x.toString)
      }}
      val headers = scala.collection.mutable.Map[String, String]("Content-Type" -> "text/xml; charset=utf-8") ++
        (action map { x => "SOAPAction" -> """"%s"""".format(x)})

      val headersList = headers.foldLeft(List[(String, String)]())((in, hdr) => hdr :: in)
      
      val Future = WS.url(address.toString).withHeaders(headersList:_*).post(r map {_.toString} getOrElse {""})
      Future.map(response => scalaxb.fromXML[Envelope](scala.xml.XML.loadString(response.body)))
      
    }

    def requestResponse(body: scala.xml.NodeSeq, headers: scala.xml.NodeSeq, scope: scala.xml.NamespaceBinding,
                        address: java.net.URI, webMethod: String, action: Option[java.net.URI]):
        Future[Either[Soap11Fault[Detail], (scala.xml.NodeSeq, scala.xml.NodeSeq)]] = {
      val bodyRecords = body.toSeq map { DataRecord(None, None, _) }
      val headerOption = headers.toSeq.headOption map { _ =>
        Header(headers.toSeq map {DataRecord(None, None, _)}, Map())
      }
      val envelope = Envelope(headerOption, Body(bodyRecords, Map()), Nil, Map())
      buildResponse(soapRequest(Some(envelope), scope, address, webMethod, action))
    }

    def soapResponse(location: Option[String], params: Map[String, Any],
                     address: java.net.URI, webMethod: String, action: Option[java.net.URI]):
        Future[Either[Soap11Fault[Detail], (scala.xml.NodeSeq, scala.xml.NodeSeq)]] = {
      buildResponse(soapRequest(None, scala.xml.TopScope, address, webMethod, action))
    }

    def buildResponse(origSoapResponse: Future[Envelope]):
        Future[Either[Soap11Fault[Detail], (scala.xml.NodeSeq, scala.xml.NodeSeq)]] = origSoapResponse.map( soapResponse =>{
      val header: scala.xml.NodeSeq =
        soapResponse.Header.toSeq flatMap { header =>
          header.any collect {
            case DataRecord(_, _, x: scala.xml.Node) => x
          }
        }
      soapResponse.Body.any.headOption match {
        case Some(DataRecord(_, _, x: scala.xml.Elem)) if (x.label == "Fault") &&
            (x.scope.getURI(x.prefix) == SOAP_ENVELOPE11_URI) =>
          val fault = scalaxb.fromXML[soapenvelope11.Fault](x)
          Left(Soap11Fault(fault, fault.detail, header))
        case _ =>
          Right(header, soapResponse.Body.any collect {
            case DataRecord(_, _, x: scala.xml.Node) => x
          })
      }
    })
  }
}
