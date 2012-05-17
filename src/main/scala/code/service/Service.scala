package code.service

import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import net.liftweb.mapper.{KeyedMapper, KeyedMetaMapper}
import net.liftweb.http.rest.{RestContinuation, RestHelper}
import code.model.User
import net.liftweb.common.{Full, BoxOrRaw, Empty}
import net.liftweb.http.{SessionVar, Req, PlainTextResponse}
import net.liftweb.http.auth.{userRoles, AuthRole}

object Service extends RestHelper {

  object LoggedIn extends SessionVar(false)

  def login(login: String, password: String): Boolean = {
    User.authenticate(login, password) match {
      case Full(user) =>
        userRoles(AuthRole(user.role.is) :: Nil)
        LoggedIn(true)
        true
      case _ =>
        false
    }
  }

  def isLoggedIn: Boolean = {
    LoggedIn.is
  }

  def logout: Boolean = {
    LoggedIn(false)
    !isLoggedIn
  }

  // webservice authentication
  val withAuthentication: PartialFunction[Req, Unit] = {
    case _ if LoggedIn.is =>
  }

  protected def basePath: List[String] = "webservices" :: Nil

  import Extractor._

  def doLogin[T: Extractor](t: T): BoxOrRaw[Any] = {
    val login = extractField(t, "login")
    val password = extractField(t, "password")
    this.login(login openOr "", password openOr "")
  }

  serveJxa {
    basePath prefixJx {
      case "login" :: Nil XmlPost xml -> _ =>
        doLogin(xml)
      case "login" :: Nil JsonPost json -> _ =>
        doLogin(json)
      case "logout" :: Nil Get req =>
        logout
      case "state" :: Nil Get _ =>
        isLoggedIn
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
