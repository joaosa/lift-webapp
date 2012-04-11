package code.service

import akka.remote.RemoteScope
import akka.actor._
import akka.pattern.ask
import net.liftweb.mapper.{Like, OrderBy, Ascending, By}
import code.model.{Device, Subscription, User}
import akka.util.Timeout
import akka.util.duration._
import net.liftweb.common.{Empty, Box, Full}
import akka.dispatch.{Promise, Future}

sealed trait Message

case class Notification(content: String) extends Message

case class Single(content: String, target: String) extends Message

case class Broadcast(content: String) extends Message

case class Reply(content: String) extends Message

class UserActor extends Actor {
  protected def receive = {
    case Notification(m) => sender ! Reply("GOT IT")
  }
}

class NotifierActor(result: Promise[String]) extends Actor {

  //implicit val timeout = Timeout(5 seconds)

  protected def receive = {
    case Broadcast(m) =>
      for {
        devices <- devicesToNotify(Subscription.findAll(OrderBy(Subscription.id, Ascending)))
        addresses <- deviceAddresses(devices)
      } yield {
        actorNotify(addresses, m)
      }
    case Single(m, t) =>
      for {
        user <- User.find(Like(User.email, t))
        subs <- Subscription.find(By(Subscription.user, user.id.is))
        devices <- devicesToNotify(subs :: Nil)
        addresses <- deviceAddresses(devices)
      } yield {
        actorNotify(addresses, m)
      }
    case Reply(m) => result.complete(Right(m))
    case _ =>
      println("Notify: invalid message type.")
  }

  def devicesToNotify(subs: List[Subscription]): Box[List[Device]] = {
    Full(subs.flatMap {
      _.user.foreign match {
        case Full(u) => User.devices(u)
        case _ => Nil
      }
    })
  }

  // TODO: remove hardcoding
  def deviceAddresses(devices: List[Device]): Box[List[Address]] = {
    Full(devices.map(d => Address("akka", "userActorSystem", "127.0.0.1", 2553)))
  }

  private def actorNotify(addresses: List[Address], m: String) = {
    Full(addresses.map {
      addr =>
        val ref = context.actorOf(Props[UserActor].withDeploy(Deploy(scope = RemoteScope(addr))))
        ref ! Notification(m)
    })
  }

}
