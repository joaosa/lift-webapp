package code.service

import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import net.liftweb.http.rest.{RestContinuation, RestHelper}
import net.liftweb.common.Full
import net.liftweb.http.auth.{userRoles, AuthRole}
import net.liftweb.http.{SessionVar, Req, PlainTextResponse}
import code.model._
import net.liftweb.mapper.{By, KeyedMapper, KeyedMetaMapper}

sealed trait Message {
  def content: String
}

case class Notification(content: String) extends Message

case class Uni(content: String, target: String) extends Message

case class Broadcast(content: String) extends Message

case class Reply(content: String) extends Message

// TODO separate into distinct services
object Service extends RestHelper {

  def basePath: List[String] = "webservices" :: Nil

  object LoggedIn extends SessionVar(false)

  def deviceLogin(userID: String, deviceID: String, password: String): Boolean =
    User.find(By(User.email, userID)) match {
      case Full(u) =>
        u.isDeviceOwnerByID(deviceID) match {
          case true => userLogin(userID, password)
          case _ => false
        }
      case _ => false
    }

  def userLogin(userID: String, password: String): Boolean = {
    User.authenticate(userID, password) match {
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
    true
  }

  // webservice authentication
  val withAuthentication: PartialFunction[Req, Unit] = {
    case _ if LoggedIn.is =>
  }

  import Extractor._
  import Converter._

  def doLogin[T: Extractable](t: T): Message = {
    val userID = extractField(t, "login")
    val password = extractField(t, "password")
    val deviceID = extractField(t, "deviceID")

    (deviceLogin(userID openOr "", deviceID openOr "",
      password openOr ""), Device.findByID(deviceID openOr "")) match {
      case (true, Full(d)) => Reply(d.id.toString())
      case _ => Reply("Invalid Login.")
    }

  }

  def doLogout(): Message = {
    Reply(logout.toString)
  }

  serve {
    basePath prefix {
      case "login" :: Nil XmlPost xml -> _ =>
        toXmlResp(doLogin(xml))
      case "login" :: Nil JsonPost json -> _ =>
        toJsonResp(doLogin(json))
      case "logout" :: Nil XmlPost _ =>
        toXmlResp(doLogout())
      case "logout" :: Nil JsonPost _ =>
        toJsonResp(doLogout())
      case "state" :: Nil Get _ =>
        PlainTextResponse(isLoggedIn.toString)
    }
  }

  // Notify
  serve {
    basePath prefix {
      case "notify" :: who :: what :: Nil Post _ => {
        // TODO fix async
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

  import Extractor._

  def range[T: Extractable](t: T) = Full(extractField(t, "start") openOr "", extractField(t, "end") openOr "")

  // Plot

  import Plotter._

  serve {
    basePath prefix {
      case "plot" :: model :: id :: plotKind :: ind :: dep :: Nil XmlPost xml -> _ =>
        RestContinuation.async {
          satisfyRequest => {
            satisfyRequest(toXmlResp(plotToChart(Point, Full(id), Full(plotKind), Full(ind), Full(dep), range(xml))))
          }
        }
      case "plot" :: model :: id :: plotKind :: ind :: dep :: Nil JsonPost json -> _ =>
        RestContinuation.async {
          satisfyRequest => {
            satisfyRequest(toXmlResp(plotToChart(Point, Full(id), Full(plotKind), Full(ind), Full(dep), range(json))))
          }
        }
    }
  }

}

trait Service[ServiceType <: KeyedMapper[_, ServiceType]] extends RestHelper
with CRUDifiable[ServiceType]
with Plotifiable[Long, ServiceType] {
  self: KeyedMetaMapper[_, ServiceType] =>

  private def modelName: String = dbName.toLowerCase

  private def servicePath: List[String] = Service.basePath ::: modelName :: Nil

  import Viewer._
  import Extractor._
  import Converter._

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
}
