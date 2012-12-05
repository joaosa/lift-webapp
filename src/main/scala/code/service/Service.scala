package code.service

import akka.actor.{Props, ActorSystem}
import akka.dispatch.Promise
import net.liftweb.http.rest.{RestContinuation, RestHelper}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.http.auth.{userRoles, AuthRole}
import net.liftweb.http.{LiftRules, SessionVar, Req, PlainTextResponse}
import code.model._
import net.liftweb.mapper.By

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
            satisfyRequest(toJsonResp(plotToChart(Point, Full(id), Full(plotKind), Full(ind), Full(dep), range(json))))
          }
        }
    }
  }

}

object Filer extends Service {

  protected def path = "file" :: Nil

  import Converter._
  import Viewer._
  import scalax.io._
  import scalax.io.JavaConverters._
  import sun.misc.{BASE64Encoder, BASE64Decoder}
  import java.io.File
  import scala.sys.process._

  def getFilePath(fileID: String): Box[String] = {
    LiftRules.getResource("/toserve/" + fileID) match {
      case Full(f) => Full(f.getPath)
      case _ => Empty
    }
  }

  def fromFile(fileID: String): String = {
    val data = Data.create.kind("RAW").saveMe()
    getFilePath(fileID) match {
      case Full(f) =>
        val blockSize = 2048 * 1024
        val blocks = new File(f).asInput.bytes.grouped(blockSize)
        val zeroBlock = new Array[Byte](blockSize).toSeq

        blocks foreach {
          case block if block != zeroBlock =>
            val codedValue = new BASE64Encoder().encodeBuffer(block.toArray)
            Raw.create.value(codedValue).data(data.id.is).save()
          case _ => ()
        }
        "File dumped to " + data.id.is
      case _ => "Invalid fileID."
    }
  }

  def toFile(dataID: String, fileID: String): String = {
    (Data.find(dataID.toLong), getFilePath(fileID)) match {
      case (Full(d), Full(f)) =>
        val output: Output = Resource.fromFile(f)
        for {
          processor <- output.outputProcessor
          out = processor.asOutput
        } {
          d.rawValues().map {
            v =>
              println("Getting: " + v)
              val b = new BASE64Decoder().decodeBuffer(v)
              out.write(b)
          }
        }
        "Data dumped."
      case (Empty, _) => "Invalid dataID."
      case (_, Empty) => "Invalid fileID."
      case _ => "Unknown failure."
    }
  }

  def processFile(processor: String, fileID: String, recordNum: Int): String = {
    (getFilePath(processor), getFilePath(fileID)) match {
      case (Full(p), Full(f)) =>
        println("p " + p)
        println("f " + f)
        (p + " " + f + " " + recordNum).! match {
          case 0 => "File processed."
          case _ => "Processing failure."
        }
      case (Empty, _) => "Invalid processor."
      case (_, Empty) => "Invalid fileID."
      case _ => "Unknown failure."
    }
  }

  def doFromFile(fileID: String): Message =
    Reply(("answer:", fromFile("RECORD-DATA.BIN")) :: Nil)

  def doToFile(dataID: String, fileID: String): Message =
    Reply(("answer:", toFile(dataID, "RECORD.BIN")) :: Nil)

  def doProcessFile(fileID: String): Message =
    Reply(("answer:", processFile("hmsp", "RECORD.BIN", 0)) :: Nil)

  def doGetFile(fileID: String): Message =
    Reply(("answer:", getFilePath("f.hr") match {
      case Full(f) => Resource.fromFile(f).string
      case _ => "Invalid fileID."
    }) :: Nil)

  serve {
    servicePath prefix {
      case "from" :: fileID :: Nil XmlPost xml -> _ => toXmlResp(toView(doFromFile(fileID)))
      case "from" :: fileID :: Nil JsonPost json -> _ => toJsonResp(toView(doFromFile(fileID)))
    }
  }

  serve {
    servicePath prefix {
      case "to" :: dataID :: fileID :: Nil XmlPost xml -> _ => toXmlResp(toView(doToFile(dataID, fileID)))
      case "to" :: dataID :: fileID :: Nil JsonPost json -> _ => toJsonResp(toView(doToFile(dataID, fileID)))
    }
  }

  serve {
    servicePath prefix {
      case "process" :: model :: id :: Nil XmlPost xml -> _ => toXmlResp(toView(doProcessFile(id)))
      case "process" :: model :: id :: Nil JsonPost json -> _ => toJsonResp(toView(doProcessFile(id)))
    }
  }

  serve {
    servicePath prefix {
      case "get" :: model :: id :: Nil XmlPost xml -> _ => toXmlResp(toView(doGetFile(id)))
      case "get" :: model :: id :: Nil JsonPost json -> _ => toJsonResp(toView(doGetFile(id)))
    }
  }
}
