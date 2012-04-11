package code.service

import net.liftweb.json.{JString, JValue}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.FieldError
import xml.NodeSeq
import net.liftweb.http.rest.RestHelper
import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import net.liftweb.mapper.{BaseMapper, KeyedMapper, KeyedMetaMapper}

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

  serve {
    val toListView = implicitly[Viewable[BaseMapper]].list _
    servicePath prefix {
      // list
      case Nil XmlGet _ =>
        for (items <- list) yield implicitly[Convertable[List[View]]].toXmlResp(toListView(items))
      case Nil JsonGet _ =>
        for (items <- list) yield implicitly[Convertable[List[View]]].toJsonResp(toListView(items))
    }
  }

  serve {
    val toView = implicitly[Viewable[Either[List[FieldError], BaseMapper]]].toView _
    servicePath prefix {
      // create
      case Nil XmlPut xml -> _ =>
        implicitly[Convertable[View]].toXmlResp(toView(create(extract, xml)))
      case Nil JsonPut json -> _ =>
        implicitly[Convertable[View]].toXmlResp(toView(create(extract, json)))

      // update
      case id :: Nil XmlPost xml -> _ =>
        implicitly[Convertable[View]].toXmlResp(toView(update(id, extract, xml)))
      case id :: Nil JsonPost json -> _ =>
        implicitly[Convertable[View]].toXmlResp(toView(update(id, extract, json)))
    }
  }

  serve {
    val toView = implicitly[Viewable[BaseMapper]].toView _
    servicePath prefix {
      // read
      case id :: Nil XmlGet _ =>
        for (item <- read(id)) yield implicitly[Convertable[View]].toXmlResp(toView(item))
      case id :: Nil JsonGet _ =>
        for (item <- read(id)) yield implicitly[Convertable[View]].toJsonResp(toView(item))

      // delete
      case id :: Nil XmlDelete _ =>
        for (item <- delete(id)) yield implicitly[Convertable[View]].toXmlResp(toView(item))
      case id :: Nil JsonDelete _ =>
        for (item <- delete(id)) yield implicitly[Convertable[View]].toJsonResp(toView(item))
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

        val system = ActorSystem()
        implicit val timeout = new Timeout(1000 milliseconds)
        val p = Promise[String]()(system.dispatcher)
        p onComplete {
          case Right(result) => Reply(result)
          case Left(error) => new Error(error.getMessage)
        }
        val notifier = system.actorOf(Props(new NotifierActor(p)))
        ask(notifier, Uni(what, who))
      }
    }
  }

}
