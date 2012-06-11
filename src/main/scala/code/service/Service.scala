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
  def content: List[(String, String)]
}

case class Notification(content: List[(String, String)]) extends Message

case class Uni(content: List[(String, String)], target: String) extends Message

case class Broadcast(content: List[(String, String)]) extends Message

case class Reply(content: List[(String, String)]) extends Message

// TODO separate into distinct services
object Service extends RestHelper {

  def basePath: List[String] = "webservices" :: Nil

  object LoggedIn extends SessionVar(false)

  def deviceLogin(userID: String,
                  deviceID: String,
                  password: String): Boolean =
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
  import Viewer._

  def doLogin[T: Extractable](t: T): Message = {
    val authenticated = (extractField(t, "userID"),
      extractField(t, "deviceID"),
      extractField(t, "password")) match {
      case (Full(u), Full(d), Full(p)) => deviceLogin(u, d, p)
      case _ => false
    }

    (authenticated,
      Device.findByID(extractField(t, "deviceID") openOr ""),
      User.findByEmail(extractField(t, "userID") openOr "")) match {
      case (true, Full(d), Full(u)) =>
        Reply(("id", d.id.toString()) ::("role", u.role.is) :: Nil)
      case _ => Reply(("error", "Invalid Login.") :: Nil)
    }

  }

  def doLogout(): Message = {
    Reply(("logout", logout.toString) :: Nil)
  }

  serve {
    basePath prefix {
      case "login" :: Nil XmlPost xml -> _ =>
        toXmlResp(toView(doLogin(xml)))
      case "login" :: Nil JsonPost json -> _ =>
        toJsonResp(toView(doLogin(json)))
      case "logout" :: Nil XmlPost xml -> _ =>
        toXmlResp(toView(doLogout()))
      case "logout" :: Nil JsonPost json -> _ =>
        toJsonResp(toView(doLogout()))
    }
  }

  serve {
    basePath prefix {
      case "state" :: Nil XmlGet _ =>
        PlainTextResponse(isLoggedIn.toString)
      case "state" :: Nil JsonGet _ =>
        PlainTextResponse(isLoggedIn.toString)
    }
  }

  // Notify
  serve {
    basePath prefix {
      case "notify" :: who :: what :: Nil Post _ => {
        // TODO fix async
        // TODO support xml as well
        RestContinuation.async {
          satisfyRequest => {
            val system = ActorSystem()
            val p = Promise[List[(String, String)]]()(system.dispatcher)
            p onComplete {
              case Right(result) =>
                () => satisfyRequest(toJsonResp(toView(Reply(result))))
              case Left(error) =>
                () => satisfyRequest(PlainTextResponse(error.getMessage))
            }
            val notifier = system.actorOf(Props(new NotifierActor(p)))
            notifier ! Uni(("content", what) :: Nil, who)
          }
        }
      }
    }
  }

  import Extractor._

  def range[T: Extractable](t: T) = Full(extractField(t, "start") openOr "", extractField(t, "end") openOr "")

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

  import Extractor._
  import Converter._
  import Viewer._

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
