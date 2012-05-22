package code.snippet

import scala.xml.NodeSeq
import net.liftweb.common.Full
import net.liftweb.util
import util.Helpers.strToCssBindPromoter
import reactive.web.html.{TextInput, Button}
import reactive.Observing
import reactive.web.RElem.rElemToNsFunc
import code.model._
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds.Script
import reactive.web.html.Select
import reactive.Val
import reactive.web.Cell

class Plotter extends Observing {

  // TODO remove this check
  val plotter = S.attr("plot") match {
    case Full("users") => User
    case Full("subscriptions") => Subscription
    case Full("devices") => Device
    case Full("messages") => Message
    case Full("data") => Data
    case _ => Point
  }

  def getInd(plotType: String): Seq[String] = {
    plotType match {
      case "group" => plotter.allFields.map(_.name)
      case "time" => "date" :: Nil
      case _ => Nil
    }
  }

  def getDep(plotType: String): Seq[String] = {
    plotType match {
      case "group" => "count" :: Nil
      case "time" => plotter.allFields.map(_.name)
      case _ => Nil
    }
  }

  val trigger = Button("Choose") {}

  val kind = Select(Val(List("blank", "group", "time", "sine")))
  kind.selectedIndex() = Some(2)

  val ind = Select(Val(getInd(kind.selectedItem.value getOrElse "")))
  ind.selectedIndex() = Some(0)

  val dep = Select(Val(getDep(kind.selectedItem.value getOrElse "")))
  dep.selectedIndex() = Some(2)

  import code.helper.Formatter._

  val start = TextInput()
  start.value() = format(now.minusDays(1))
  start.value updateOn trigger.click

  val end = TextInput()
  end.value() = format(now)
  end.value updateOn trigger.click

  def render =
    "#plotKind" #> kind &
      "#ind" #> ind &
      "#dep" #> dep &
      "#start" #> start &
      "#end" #> end &
      "#trigger" #> trigger &
      "#results" #> Cell {
        for {
          kindName <- kind.selectedItem
          indName <- ind.selectedItem
          depName <- dep.selectedItem
          indMin <- start.value
          indMax <- end.value
        } yield {
          _: NodeSeq =>
            Script(plotter.plotToJs(kindName getOrElse "",
              indName getOrElse "", depName getOrElse "", Full(indMin, indMax)))
        }
      }
}
