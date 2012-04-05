package code.snippet

import reactive.Observing
import net.liftweb.util
import util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.html.Button
import net.liftweb.http.S
import code.service.{ Notify, Notification }

class Notifier extends Observing {

  val trigger = Button("Send") {
    S.notice("Sent: " + what.value.value + " to " + who.value.value)
    val r = Notify !< new Notification(what.value.value, who.value.value)
    S.notice("Got: " + r)
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
