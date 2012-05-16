package code.service

import net.liftweb.common.Empty
import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import net.liftweb.mapper.{KeyedMapper, KeyedMetaMapper}
import net.liftweb.http.rest.{RestContinuation, RestHelper}
import net.liftweb.http.{PlainTextResponse, SessionVar}

object Test extends RestHelper {
  serveJxa {
    Service.basePath prefixJx {
      case "protected" :: Nil Get _ =>
        "Ohh, secret"
    }
  }
}

object Service extends RestHelper {

  object LoggedIn extends SessionVar(false)

  def basePath: List[String] = "webservices" :: Nil

  serveJxa {
    basePath prefixJx {
      case "login" :: Nil Get req =>
        LoggedIn(true)
      case "logout" :: Nil Get req =>
        LoggedIn(false)
      case "state" :: Nil Get _ =>
        LoggedIn.is
    }
  }

  // Notify
  serve {
    basePath prefix {
      case "notify" :: who :: what :: Nil Post _ => {
        RestContinuation.async {
          satisfyRequest => {
            val system = ActorSystem()
            val p = Promise[String]()(system.dispatcher)
            p onComplete {
              case Right(result) =>
                () => satisfyRequest(PlainTextResponse(result))
              case Left(error) =>
                () => satisfyRequest(PlainTextResponse(error.getMessage))
            }
            val notifier = system.actorOf(Props(new NotifierActor(p)))
            notifier ! Uni(what, who)
          }
        }
      }
    }
  }

}

trait Service[ServiceType <: KeyedMapper[_, ServiceType]] extends RestHelper
with CRUDifiable[ServiceType]
with Plottable[Long, ServiceType] {
  self: ServiceType with KeyedMetaMapper[_, ServiceType] =>

  private def modelName: String = dbName.toLowerCase

  private def servicePath: List[String] = Service.basePath ::: modelName :: Nil

  import Viewable._
  import Convertable._

  def toXmlResp[T: Convertable](t: T) = implicitly[Convertable[T]].toXmlResp(t)

  def toJsonResp[T: Convertable](t: T) = implicitly[Convertable[T]].toJsonResp(t)

  serve {
    servicePath prefix {
      // create
      case Nil XmlPut xml -> _ =>
        toXmlResp(toView(create(xml)))
      case Nil JsonPut json -> _ =>
        toJsonResp(toView(create(json)))

      // create list
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
}
