package code.snippet

import reactive.Observing
import net.liftweb.util
import util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.html.Button
import net.liftweb.http.S
import akka.actor.{ActorSystem, Props}
import code.service.{NotifierActor, Uni}
import akka.dispatch.Promise

class Notifier extends Observing {

  val system = ActorSystem("notifierActorSystem")

  // TODO notices aren't shown
  def alert(e: Either[Throwable, String]) {
    e match {
      case Right(result) =>
        println("Got: " + result)
        S.notice("Got: " + result)
      case Left(error) =>
        println("Got: " + error)
        S.notice("Got: " + error)
    }
  }

  val trigger = Button("Send") {
    S.notice("Sent: " + what.value.value + " to " + who.value.value)

    val p = Promise[String]()(system.dispatcher)
    p.onComplete(alert)
    val notifier = system.actorOf(Props(new NotifierActor(p)))
    notifier ! Uni(what.value.value, who.value.value)
  }

  val who = TextInput()
  who.value updateOn trigger.click

  val what = TextInput()
  what.value updateOn trigger.click

  def render = {
    "#who" #> who &
      "#what" #> what &
      "#trigger" #> trigger
  }
}
