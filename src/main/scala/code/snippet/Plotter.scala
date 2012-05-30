package code.snippet

import scala.xml.NodeSeq
import net.liftweb.common.Full
import net.liftweb.util
import net.liftweb.util.Helpers.now
import util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.RElem.rElemToNsFunc
import code.model._
import net.liftweb.http.S
import reactive.web.html.Select
import net.liftweb.http.js.JsCmds.Script
import reactive.web.Cell
import reactive._

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

  def getIndItems(kind: Option[String]): Seq[String] = {
    kind match {
      case Some("group") => plotter.allFields.map(_.name)
      case Some("time") => "date" :: Nil
      case _ => "---" :: Nil
    }
  }

  def getDepItems(kind: Option[String]): Seq[String] = {
    kind match {
      case Some("group") => "count" :: Nil
      case Some("time") => plotter.allFields.map(_.name)
      case _ => "---" :: Nil
    }
  }

  def plot: Signal[NodeSeq => NodeSeq] = {
    for {
      kindName <- kind.selectedItem
      indName <- ind.selectedItem
      depName <- dep.selectedItem
      indMin <- start.value
      indMax <- end.value
    } yield {
      _: NodeSeq =>
        Script(plotter.plotToJs(kindName getOrElse "",
          indName getOrElse "", depName getOrElse "", (indMin, indMax)))
    }
  }

  val kind = Select(Val(Seq("blank", "group", "time", "sine")))

  val ind = Select(kind.selectedItem.map(s => getIndItems(s)))

  val dep = Select(kind.selectedItem.map(s => getDepItems(s)))

  import code.helper.Formatter._

  val start = TextInput()
  start.value() = format(now.minusDays(1))

  val end = TextInput()
  end.value() = format(now)

  val results = Cell(plot)

  def render =
    "#plotKind" #> kind &
      "#ind" #> ind &
      "#dep" #> dep &
      "#start" #> start &
      "#end" #> end &
      "#results" #> results
}
