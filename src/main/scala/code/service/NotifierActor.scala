package code.service

import akka.remote.RemoteScope
import akka.actor._
import net.liftweb.mapper.{OrderBy, Ascending}
import code.model.{Device, Subscription, User}
import net.liftweb.common.{Box, Full}
import akka.dispatch.Promise

class UserActor extends Actor {
  protected def receive = {
    case Notification(m) => sender ! Reply(("notification", "GOT IT") :: Nil)
  }
}

class NotifierActor(result: Promise[List[(String, String)]]) extends Actor {

  protected def receive = {
    case Broadcast(content) =>
      for {
        devices <- devicesToNotify(Subscription.findAll(OrderBy(Subscription.id, Ascending)))
        addresses <- deviceAddresses(devices :: Nil)
      } yield {
        actorNotify(addresses, content)
      }
    case Uni(content, t) =>
      for {
        subs <- Subscription.findByEmail(t)
        devices <- devicesToNotify(subs :: Nil)
        addresses <- deviceAddresses(devices :: Nil)
      } yield {
        actorNotify(addresses, content)
      }
    case Reply(content) =>
      result.complete(Right(content))
    case _ =>
      println("Notify: invalid message type.")
  }

  def devicesToNotify(subs: List[Subscription]): List[Device] = {
    subs.flatMap {
      _.user.foreign match {
        case Full(u) => User.devices(u)
        case _ => Nil
      }
    }
  }

  // TODO: remove hardcoding
  def deviceAddresses(devices: List[Device]): Box[List[Address]] = {
    Full(devices.map(d => Address("akka", "userActorSystem", "127.0.0.1", 2553)))
  }

  private def actorNotify(addresses: List[Address], content: List[(String, String)]) = {
    addresses.map {
      addr =>
        val ref = context.actorOf(Props[UserActor].withDeploy(Deploy(scope = RemoteScope(addr))))
        ref ! Notification(content)
    }
  }

}
