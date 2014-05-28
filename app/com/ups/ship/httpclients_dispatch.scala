package scalaxb

trait DispatchHttpClients extends HttpClients {
  val httpClient = new DispatchHttpClient {}

  trait DispatchHttpClient extends HttpClient {
   // import dispatch._

    def request(in: String, address: java.net.URI, headers: Map[String, String]): String = {
     /* val http = new Http
     / Http(url(address.toString) << (in) <:< headers OK as.String)()
      */  // Only here for reference
      ""
    }
  }
}
