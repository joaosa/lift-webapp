package code.service

import net.liftweb.json.Xml
import net.liftweb.http.rest.RestHelper
import scala.xml.Node
import net.liftweb.json.JsonAST.JValue

trait Convertable extends RestHelper {
  def toXml: Node
  def toJson: JValue = Xml.toJson(toXml)
}

sealed trait Message extends Convertable {
  def content: String
  override def toString() = content
  override def toXml = <message>{ toString() }</message>
}

class Error(param: String) extends Message {
  def content = "Error %s"
  override def toString() = super.toString format param
}
case class Missing(param: String) extends Error(param) {
  content + "%s Parameter Missing."
}
case class NotFound(param: String) extends Error(param) {
  content + "%s Not Found."
}
case class Invalid(param: String) extends Error(param) {
  content + "Invalid Parameters."
}

case class Notification(content: String, target: String) extends Message
case class Broadcast(content: String) extends Message
case class Reply(content: String) extends Message
