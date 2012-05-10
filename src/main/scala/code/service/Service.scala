package code.service

import net.liftweb.common.Empty
import net.liftweb.http.rest.RestHelper
import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import net.liftweb.mapper.{KeyedMapper, KeyedMetaMapper}

trait Service[ServiceType <: KeyedMapper[_, ServiceType]] extends RestHelper
with CRUDifiable[ServiceType]
with Plottable[Long, ServiceType] {
  self: ServiceType with KeyedMetaMapper[_, ServiceType] =>

  private def basePath: List[String] = "webservices" :: Nil

  private def modelName: String = dbName.toLowerCase

  private def servicePath: List[String] = basePath ::: modelName :: Nil

  import Viewable._
  import Convertable._

  def toXmlResp[T: Convertable](t: T) = implicitly[Convertable[T]].toXmlResp(t)

  def toJsonResp[T: Convertable](t: T) = implicitly[Convertable[T]].toJsonResp(t)

  def toListView[T: Viewable](t: List[T]) = implicitly[Viewable[T]].list(t)

  def toView[T: Viewable](t: T) = implicitly[Viewable[T]].toView(t)

  serve {
    servicePath prefix {
      // create
      case Nil XmlPut xml -> _ =>
        toXmlResp(toView(create(xml)))
      case Nil JsonPut json -> _ =>
        toJsonResp(toView(create(json)))

      // create readAll
      case "list" :: Nil XmlPut xml -> _ =>
        toXmlResp(toListView(createList(xml, modelName)))
      case "list" :: Nil JsonPut json -> _ =>
        toJsonResp(toListView(createList(json, modelName)))
    }
  }

  serve {
    servicePath prefix {
      // update
      case id :: Nil XmlPost xml -> _ =>
        toXmlResp(toView(update(id, xml)))
      case id :: Nil JsonPost json -> _ =>
        toJsonResp(toView(update(id, json)))
    }
  }

  serve {
    servicePath prefix {
      // read
      case id :: Nil XmlGet _ =>
        for (item <- read(id)) yield toXmlResp(toView(item))
      case id :: Nil JsonGet _ =>
        for (item <- read(id)) yield toJsonResp(toView(item))

      // read all
      case Nil XmlGet _ =>
        for (items <- readAll) yield toXmlResp(toListView(items))
      case Nil JsonGet _ =>
        for (items <- readAll) yield toJsonResp(toListView(items))
    }
  }

  serve {
    servicePath prefix {
      // delete
      case id :: Nil XmlDelete _ =>
        for (item <- delete(id)) yield toXmlResp(toView(item))
      case id :: Nil JsonDelete _ =>
        for (item <- delete(id)) yield toJsonResp(toView(item))
    }
  }

  // Plot
  serve {
    servicePath prefix {
      case "plot" :: plotKind :: ind :: dep :: Nil XmlGet _ =>
        toXmlResp(plot(plotKind, (ind, dep), Empty))
      case "plot" :: plotKind :: ind :: dep :: Nil JsonGet _ =>
        toJsonResp(plot(plotKind, (ind, dep), Empty))
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
