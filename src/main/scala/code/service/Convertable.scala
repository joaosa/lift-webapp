package code.service

import net.liftweb.http.rest.RestHelper
import net.liftweb.json.{Extraction, Xml}
import sun.misc.BASE64Encoder
import net.liftweb.http.{JsonResponse, XmlResponse, InMemoryResponse}
import net.liftweb.http.js.JsExp
import xml._
import org.jfree.chart.{JFreeChart, ChartUtilities}
import net.liftweb.json.JsonAST.{JArray, JObject, JField}

trait Convertable[T] extends RestHelper {
  def toXml(t: T): Node

  def toXmlResp(t: T) = XmlResponse(toXml(t))

  def toJson(t: T): JsExp = Xml.toJson(toXml(t)) transform {
    case JObject(List(JField("list", JObject(List(JField(n, o)))))) =>
      o match {
        case JObject(_) =>
          JObject(List(JField("list", JObject(List(JField(n, JArray(List(o))))))))
        case _ => JObject(List(JField("list", JObject(List(JField(n, o))))))
      }
  }

  def toJsonResp(t: T) = JsonResponse(toJson(t))
}

object Converter {

  implicit def toXmlResp[T: Convertable](t: T) = implicitly[Convertable[T]].toXmlResp(t)

  implicit def toJsonResp[T: Convertable](t: T) = implicitly[Convertable[T]].toJsonResp(t)

  implicit object View extends Convertable[View] {
    def toXml(t: View) = Elem(null, t.name, Null, TopScope,
      Xml.toXml(Extraction.decompose(t.items.toMap)): _*)
  }

  implicit object ViewList extends Convertable[List[View]] {
    def toXml(t: List[View]) = <list>{t.map(View.toXml)}</list>
  }

  implicit object JFreeChart extends Convertable[JFreeChart] {
    def toXml(t: JFreeChart) = encode(t)

    def toResp(t: JFreeChart) = {
      val png = toPNG(t)
      InMemoryResponse(png,
        List("Content-Type" -> "application/image",
          "Content-Length" -> png.length.toString), Nil, 200)
    }

    private def toPNG(t: JFreeChart): Array[Byte] = {
      ChartUtilities.encodeAsPNG(t.createBufferedImage(500, 500))
    }

    private def encode(t: JFreeChart) = <plot>{new BASE64Encoder().encode(toPNG(t))}</plot>
  }

}
