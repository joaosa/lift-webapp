package code.snippet

import scala.xml.NodeSeq
import net.liftweb.common.Full
import net.liftweb.util
import util.Helpers.strToCssBindPromoter
import reactive.web.html.{ TextInput, Button }
import reactive.Observing
import reactive.web.RElem.rElemToNsFunc
import code.helper.Date.{now, toString}
import code.model._
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds.Script
import reactive.web.html.Select
import reactive.Val

class Plotter extends Observing {

  val plotter = S.attr("plot") match {
    case Full("users") => User
    case Full("subscriptions") => Subscription
    case Full("devices") => Device
    case Full("messages") => Message
    case Full("data") => Data
    case _ => Point
  }

  val trigger = Button("Plot") {}

  val kind = Select(Val(List("blank", "group", "time", "sine")))
  kind.selectedIndex() = Some(0)

  val ind = TextInput()
  ind.value updateOn trigger.click

  val dep = TextInput()
  dep.value updateOn trigger.click

  val start = TextInput()
  start.value() = now.minusDays(1)
  start.value updateOn trigger.click

  val end = TextInput()
  end.value() = now
  end.value updateOn trigger.click

  def render = {
    "#plotKind" #> kind &
      "#ind" #> ind &
      "#dep" #> dep &
      "#start" #> start &
      "#end" #> end &
      "#trigger" #> trigger &
      "#results" #> reactive.web.Cell(
        for {
          kindName <- kind.selectedItem
          indName <- ind.value
          depName <- dep.value
          indMin <- start.value
          indMax <- end.value
        } yield {
          _: NodeSeq =>
            Script(plotter.plot(kindName getOrElse "", (indName, depName), Full(indMin, indMax))
              .toJs("placeholder"))
        })
  }
}
