package code.service

import net.liftweb.json.{JString, JValue}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.FieldError
import net.liftweb.mapper.{Mapper, KeyedMapper, KeyedMetaMapper}
import xml.NodeSeq
import net.liftweb.http.rest.{JsonXmlSelect, RestHelper, JsonSelect, XmlSelect}
import net.liftweb.http.{LiftResponse, Req}
import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

trait Service[ServiceType <: KeyedMapper[_, ServiceType]] extends RestHelper
  with CRUDifiable[ServiceType]
  with Plottable[Long, ServiceType] {
  self: ServiceType with KeyedMetaMapper[_, ServiceType] =>

  private def basePath: List[String] = "webservices" :: Nil
  private def servicePath: List[String] = basePath ::: _dbTableNameLC :: Nil

  def extract(data: Any, field: String): Box[String] = {
    data match {
      case data: JValue => Box(for (JString(str) <- data \ field) yield str)
      case data: NodeSeq => Full((data \ field).text)
      case _ => Empty
    }
  }

  import Viewable._
  import Convertable._

  def doRead(item: Mapper[_], c: View => LiftResponse)(implicit m: Viewable[Mapper[_]]) = {
    c(m.toView(item))
  }

  def doSave(item: Mapper[_], c: View => LiftResponse, cl: List[View] => LiftResponse)(implicit m: Viewable[Mapper[_]],
                                lfe: Viewable[FieldError]): LiftResponse = {
    item.validate match {
      case Nil =>
        item.save()
        c(m.toView(item))
      case xs =>
        cl(lfe.list(xs))
    }
  }

  serve {
    servicePath prefix {
      // read
      case id :: Nil XmlGet _ =>
        for (item <- read(id)) yield doRead(item, implicitly[Convertable[View]].toXmlResp)
      case id :: Nil JsonGet _ =>
        for (item <- read(id)) yield doRead(item, implicitly[Convertable[View]].toJsonResp)
    }
  }

  serve {
    servicePath prefix {
      // list
      case Nil XmlGet _ =>
        for (items <- list) yield implicitly[Convertable[List[View]]].toXmlResp(implicitly[Viewable[Mapper[_]]].list(items))
      case Nil JsonGet _ =>
        for (items <- list) yield implicitly[Convertable[List[View]]].toJsonResp(implicitly[Viewable[Mapper[_]]].list(items))

      // delete
      case id :: Nil XmlDelete _ =>
        for (item <- delete(id)) yield implicitly[Convertable[View]].toXmlResp(implicitly[Viewable[Mapper[_]]].toView(item))
      case id :: Nil JsonDelete _ =>
        for (item <- delete(id)) yield implicitly[Convertable[View]].toJsonResp(implicitly[Viewable[Mapper[_]]].toView(item))
    }
  }

  serve {
    servicePath prefix {
      // create
      case Nil XmlPut xml -> _ =>
        for (item <- create(extract, xml)) yield doSave(item, implicitly[Convertable[View]].toXmlResp, implicitly[Convertable[List[View]]].toXmlResp)
      case Nil JsonPut json -> _ =>
        for (item <- create(extract, json)) yield doSave(item, implicitly[Convertable[View]].toJsonResp, implicitly[Convertable[List[View]]].toJsonResp)

      // update
      case id :: Nil XmlPost xml -> _ =>
        for (item <- update(id, extract, xml)) yield doSave(item, implicitly[Convertable[View]].toXmlResp, implicitly[Convertable[List[View]]].toXmlResp)
      case id :: Nil JsonPost json -> _ =>
        for (item <- update(id, extract, json)) yield doSave(item, implicitly[Convertable[View]].toJsonResp, implicitly[Convertable[List[View]]].toJsonResp)
    }
  }

  // Plot
  serve {
    servicePath prefix {
      case "plot" :: plotKind :: ind :: dep :: Nil XmlGet _ =>
        implicitly[Convertable[Chart]].toXmlResp(plot(plotKind, (ind, dep), Empty))
      case "plot" :: plotKind :: ind :: dep :: Nil JsonGet _ =>
        implicitly[Convertable[Chart]].toJsonResp(plot(plotKind, (ind, dep), Empty))
    }
  }

  // Notify
  serveJxa {
    servicePath prefixJx {
      case "notify" :: who :: what :: Nil Post _ => {
        val system = ActorSystem("notifierActorSystem")
        implicit val timeout = Timeout(5 seconds)
        val notifier = system.actorOf(Props[NotifierActor], name = "Notifier")
        ask(notifier, Single(what, who)).map(_.toString) onComplete {
          case Right(result) => Reply(result)
          case Left(error) => new Error(error.getMessage)
        }
      }
    }
  }

}
