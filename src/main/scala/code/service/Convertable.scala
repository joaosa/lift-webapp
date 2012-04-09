package code.service

import net.liftweb.http.rest.RestHelper
import net.liftweb.json.{Extraction, Xml}
import sun.misc.BASE64Encoder
import org.jfree.chart.ChartUtilities
import net.liftweb.http.{JsonResponse, XmlResponse, InMemoryResponse}
import xml.{Node, Elem, Null, TopScope}
import net.liftweb.http.js.JsExp

trait Convertable[T] extends RestHelper {
  def toXml(t: T): Node

  def toXmlResp(t: T) = XmlResponse(toXml(t))

  def toJson(t: T): JsExp = Xml.toJson(toXml(t))

  def toJsonResp(t: T) = JsonResponse(toJson(t))
}

object Convertable {

  implicit object Message extends Convertable[Message] {
    def toXml(t: Message) = <message>
      {t.toString()}
    </message>
  }

  implicit object View extends Convertable[View] {
    def toXml(t: View) = Elem(null, t.name, Null, TopScope,
      Xml.toXml(Extraction.decompose(t.items.toMap)).toList: _*)
  }

  implicit object ViewList extends Convertable[List[View]] {
    def toXml(t: List[View]) = <list>
      {t.map(View.toXml)}
    </list>
  }

  implicit object Chart extends Convertable[Chart] {
    def toXml(t: Chart) = encode(t)

    def toResp(c: Chart) = InMemoryResponse(toPNG(c),
      List("Content-Type" -> "application/image",
        "Content-Length" -> toPNG(c).length.toString), Nil, 200)

    private def toPNG(c: Chart): Array[Byte] =
      ChartUtilities.encodeAsPNG(
        c.toExport.createBufferedImage(500, 500))

    private def encode(t: Chart) = <plot>
      {new BASE64Encoder().encode(toPNG(t))}
    </plot>
  }

}

class Message(content: String) {

  override def toString = content
}

class Error(param: String) extends Message("Error %s") {

  override def toString = super.toString format param
}

case class Missing(param: String) extends Error("%s Parameter Missing.")

case class NotFound(param: String) extends Error("%s Not Found.")

case class Invalid(param: String) extends Error("Invalid Parameters.")

case class Notification(content: String, target: String) extends Message(content)

case class Broadcast(content: String) extends Message(content)

case class Reply(content: String) extends Message(content)
