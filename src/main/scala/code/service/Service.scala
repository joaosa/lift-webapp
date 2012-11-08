package code.service

import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import net.liftweb.http.rest.{RestContinuation, RestHelper}
import net.liftweb.common.Full
import net.liftweb.http.auth.{userRoles, AuthRole}
import net.liftweb.http.{LiftRules, SessionVar, Req, PlainTextResponse}
import code.model._
import net.liftweb.mapper.{By, KeyedMapper, KeyedMetaMapper}

sealed trait Message {
  def content: List[(String, String)]
}

case class Notification(content: List[(String, String)]) extends Message

case class Uni(content: List[(String, String)], target: String) extends Message

case class Broadcast(content: List[(String, String)]) extends Message

case class Reply(content: List[(String, String)]) extends Message

trait Service extends RestHelper {
  protected def basePath: List[String] = "webservices" :: Nil

  protected def path: List[String]

  protected def servicePath: List[String] = basePath ::: path
}

object Login extends Service {

  protected def path = Nil

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
    val userID = extractField(t, "userLogin")
    val deviceID = extractField(t, "deviceLogin")

    val authenticated = (userID, deviceID,
      extractField(t, "password")) match {
      case (Full(u), Full(d), Full(p)) => deviceLogin(u, d, p)
      case _ => false
    }

    (authenticated, Device.findByID(deviceID openOr ""),
      User.findByEmail(userID openOr "")) match {
      case (true, Full(d), Full(u)) =>
        Reply(("userID", u.id.is.toString) ::
          ("deviceID", d.id.is.toString) ::
          ("role", u.role.is) :: Nil)
      case _ => Reply(("error", "Invalid Login.") :: Nil)
    }

  }

  def doLogout(): Message = {
    Reply(("logout", logout.toString) :: Nil)
  }

  serve {
    servicePath prefix {
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
    servicePath prefix {
      case "state" :: Nil XmlGet _ =>
        PlainTextResponse(isLoggedIn.toString)
      case "state" :: Nil JsonGet _ =>
        PlainTextResponse(isLoggedIn.toString)
    }
  }
}

object Notifier extends Service {

  protected def path = Nil

  import Converter._
  import Viewer._

  serve {
    servicePath prefix {
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
}

object Plotter extends Service {

  protected def path = Nil

  import Converter._
  import Extractor._
  import PlotBuilder._

  def range[T: Extractable](t: T) =
    Full(extractField(t, "start") openOr "",
      extractField(t, "end") openOr "")

  serve {

    servicePath prefix {
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

object Filer extends Service {

  protected def path = Nil

  import Converter._
  import Viewer._
  import scalax.io._
  import scalax.io.JavaConverters._

  def toFile(dataID: String): Boolean = {
    for {
      data <- Data.find(dataID.toLong)
      dir <- LiftRules.getResource("/toserve/someFile")
    } yield {
      println(dir.getPath)
      Data.raws.map { r =>
        println("in")
        val bytes = new sun.misc.BASE64Decoder().decodeBuffer(r.value.is)
        new java.io.File(dir.getPath).asOutput.write(bytes)
      }
      true
    }
    false
  }

  def doToFile(dataID: String): Message = {
    if (toFile(dataID)) {
      Reply(("answer:", "success") :: Nil)
    } else {
      Reply(("answer:", "success") :: Nil)
    }
  }

  serve {
    servicePath prefix {
      case "file" :: model :: id :: Nil XmlPost xml -> _ =>
        RestContinuation.async {
          satisfyRequest => {
            satisfyRequest(toXmlResp(toView(doToFile(id))))
          }
        }
      case "file" :: model :: id :: Nil JsonPost json -> _ =>
        RestContinuation.async {
          satisfyRequest => {
            satisfyRequest(toJsonResp(toView(doToFile(id))))
          }
        }
    }
  }

}

trait DomainService[ServiceType <: KeyedMapper[_, ServiceType]] extends Service
with CRUDifiable[ServiceType]
with Plotifiable[Long, ServiceType] {
  self: KeyedMetaMapper[_, ServiceType] =>

  def path = dbTableName :: Nil

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
        toXmlResp(toListView(createList(xml)))
      case "list" :: Nil JsonPut json -> _ =>
        toJsonResp(toListView(createList(json)))
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

      // read field
      case id :: field :: Nil XmlGet _ =>
        for (f <- readField(id, field)) yield toXmlResp(toView(f))
      case id :: field :: Nil JsonGet _ =>
        for (f <- readField(id, field)) yield toJsonResp(toView(f))

      // read all with field
      // TODO modify the method to accept only strings
      case "list" :: field :: value :: Nil XmlGet _ =>
        for (items <- readAllWithField[Long](field, value.toLong)) yield toXmlResp(toListView(items))
      case "list" :: field :: value :: Nil JsonGet _ =>
        for (items <- readAllWithField[Long](field, value.toLong)) yield toJsonResp(toListView(items))

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
