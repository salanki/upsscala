package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

}


class DelimitedStringToInt(s: String) {
  def delimitedToInt = s.replace(",","").toInt
}

object Implicits {
  implicit def implicitDelimitedStringToInt(a: String) = new DelimitedStringToInt(a)
}