package code.service

import net.liftweb.actor.LiftActor
import code.model.Subscription
import net.liftweb.mapper.OrderBy
import net.liftweb.mapper.Ascending
import net.liftweb.common.Full
import code.model.User
import dispatch._
import dispatch.nio
import scala.xml.Text
import scala.xml.NodeSeq
import net.liftweb.mapper.By

object Notify extends LiftActor {

  def messageHandler = {
    case m: Broadcast =>
      Subscription.findAll(OrderBy(Subscription.id, Ascending)).map(notifyUser(_, m)).head
    case m: Notification =>
      User.findByName(m.target) match {
        case Full(u) => Subscription.findAll(By(Subscription.user, u.id.is)).map(notifyUser(_, m)).head
        case _ => println("Notify: invalid user identification.")
      }
    case _ => println("Notify: invalid message type.")
  }

  def notifyUser(s: Subscription, m: Message) {
    s.user.foreign match {
      case Full(u) =>
        User.devices(u).map(d => send(d.address.is, d.port.is, m))
        println("Notify: " + User.name(u) + " notified.")
      case _ => println("Notify: no user to notify.")
    }
  }

  def send(addr: String, port: Int, m: Message) = {
    val http = new nio.Http
    val req = :/(addr, port)
    var response: NodeSeq = Text("")
    http(req << m.toString <> { response = _ })
    response
    //http.shutdown()
  }
}

/*object Pub {
  println("starting publishing service ..")
  val p = actorOf(new Publisher(new RedisClient("localhost", 6379))).start

  def publish(channel: String, message: String) = {
    p ! Publish(channel, message)
  }
}

class Publisher(client: RedisClient) extends Actor {
  def receive = {
    case Publish(channel, message) =>
      client.publish(channel, message)
      self.reply(true)
  }
}

object Sub {
  println("starting subscription service ..")
  val s = actorOf(new Subscriber(new RedisClient("localhost", 6379))).start
  s ! Register(callback)

  def sub(channels: String*) = {
    s ! Subscribe(channels.toArray)
  }

  def unsub(channels: String*) = {
    s ! Unsubscribe(channels.toArray)
  }

  def callback(pubsub: PubSubMessage) = pubsub match {
    case S(channel, no) => println("subscribed to " + channel + " and count = " + no)
    case U(channel, no) => println("unsubscribed from " + channel + " and count = " + no)
    case M(channel, msg) =>
      msg match {
        // exit will unsubscribe from all channels and stop subscription service
        case "exit" =>
          println("unsubscribe all ..")
          r.unsubscribe

        // message "+x" will subscribe to channel x
        case x if x startsWith "+" =>
          val s: Seq[Char] = x
          s match {
            case Seq('+', rest @ _*) => r.subscribe(rest.toString) { m => }
          }

        // message "-x" will unsubscribe from channel x
        case x if x startsWith "-" =>
          val s: Seq[Char] = x
          s match {
            case Seq('-', rest @ _*) => r.unsubscribe(rest.toString)
          }

        // other message received
        case x =>
          println("received message on channel " + channel + " as : " + x)
      }
  }
}*/
