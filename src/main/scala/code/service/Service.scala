package code.service

import scala.xml.NodeSeq
import net.liftweb.http.rest.{ RestHelper, JsonSelect, XmlSelect }
import net.liftweb.json.{ JString, JValue }
import net.liftweb.mapper.{ KeyedMapper, KeyedMetaMapper }
import net.liftweb.common.{BoxedBoxOrRaw, Box, Empty, Full}

trait Service[ServiceType <: KeyedMapper[_, ServiceType]] extends RestHelper
  with CRUDifiable[ServiceType]
  with Plottable[Long, ServiceType] {
  self: ServiceType with KeyedMetaMapper[_, ServiceType] =>

  implicit def content(m: Message): String = m.toString()

  private def basePath: List[String] = "webservices" :: Nil
  private def servicePath: List[String] = basePath ::: _dbTableNameLC :: Nil

  /*protected def imageResponse_?(in: Req): Boolean = {
    val accept = in.headers("accept")
    accept.find(_.toLowerCase.indexOf("application/image") >= 0).isDefined ||
      ((in.path.suffix equalsIgnoreCase "image") &&
        (accept.isEmpty ||
          accept.find(_.toLowerCase.indexOf("") >= 0).isDefined)) ||
          suplimentalImageResponse_?(in)
  }

  protected def suplimentalImageResponse_?(in: Req): Boolean = false

  protected trait ImageTest {
    def testResponse_?(r: Req): Boolean = imageResponse_?(r)
  }

  protected lazy val ImageReq = new TestReq with ImageTest
  protected lazy val ImageGet = new TestGet with ImageTest
  protected lazy val ImageDelete = new TestDelete with ImageTest
  protected lazy val ImagePost = new TestPost with ImageTest
  protected lazy val ImagePut = new TestPut with ImageTest

  protected trait ImageBody {
    def body(r: Req): Box[String] = r.
  }*/

  def extract(data: Any, field: String): Box[String] = {
    data match {
      case data: JValue => Box(for (JString(str) <- data \ field) yield str)
      case data: NodeSeq => Full((data \ field).text)
      case _ => Empty
    }
  }

  implicit def cvt: JxCvtPF[Convertable] = {
    case (JsonSelect, c, _) => c.toJson
    case (XmlSelect, c, _) => c.toXml
  }

  def doSubmit(item: KeyedMapper[_, _]): Convertable = {
    item.validate match {
      case Nil =>
        item.save()
        item
      case xs => xs
    }
  }

  serveJx[Convertable] {
    servicePath prefixJx {
      // list
      case Nil Get _ => list

      // create
      case Nil XmlPut xml -> _ =>
        for (item <- create(extract, xml)) yield doSubmit(item)
      case Nil JsonPut json -> _ =>
        for (item <- create(extract, json)) yield doSubmit(item)

      // read
      case id :: Nil Get _ =>
        for (item <- read(id)) yield item
    }
  }

  serveJx[Convertable] {
    servicePath prefixJx {
      // update
      case id :: Nil XmlPost xml -> _ =>
        for (item <- update(id, extract, xml)) yield doSubmit(item)
      case id :: Nil JsonPost json -> _ =>
        for (item <- update(id, extract, json)) yield doSubmit(item)

      // delete
      case id :: Nil Delete _ =>
        for (item <- delete(id)) yield item
    }
  }

  // Plot
  serveJx[Convertable] {
    servicePath prefixJx {
      case "plot" :: plotKind :: ind :: dep :: Nil Get _ => plot(plotKind, (ind, dep), Empty)
    }
  }

  // Notify
  serveJx[Convertable] {
    servicePath prefixJx {
      case "notify" :: who :: what :: Nil Post _ =>
        for (answer <- (Notify !< Notification(what, who)).get(500L)) yield Reply(answer.toString)
    }
  }
}
