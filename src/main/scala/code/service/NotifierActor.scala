package code.service

import akka.remote.RemoteScope
import akka.actor._
import net.liftweb.mapper.{Like, OrderBy, Ascending, By}
import code.model.{Device, Subscription, User}
import net.liftweb.common.{Box, Full}
import akka.dispatch.Promise

class UserActor extends Actor {
  protected def receive = {
    case Notification(m) => sender ! Reply("GOT IT")
  }
}

class NotifierActor(result: Promise[String]) extends Actor {

  protected def receive = {
    case Broadcast(m) =>
      for {
        devices <- devicesToNotify(Subscription.findAll(OrderBy(Subscription.id, Ascending)))
        addresses <- deviceAddresses(devices)
      } yield {
        actorNotify(addresses, m)
      }
    case Uni(m, t) =>
      for {
        user <- User.find(Like(User.username, t))
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
    addresses.map {
      addr =>
        val ref = context.actorOf(Props[UserActor].withDeploy(Deploy(scope = RemoteScope(addr))))
        ref ! Notification(m)
    }
  }

}
