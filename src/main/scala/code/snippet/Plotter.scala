package code.snippet

import scala.xml.NodeSeq
import net.liftweb.common.Full
import net.liftweb.util
import net.liftweb.util.Helpers.now
import util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.RElem.rElemToNsFunc
import reactive.web.html.Select
import net.liftweb.http.js.JsCmds.Script
import reactive.web.Cell
import reactive._
import code.model._
import net.liftweb.mapper.BaseMapper

class Plotter extends Observing {

  import code.service.PlotBuilder._

  def plot: Signal[NodeSeq => NodeSeq] = {
    for {
      model <- model.selectedItem
      kindName <- kind.selectedItem
      indName <- ind.selectedItem
      depName <- dep.selectedItem
      indMin <- start.value
      indMax <- end.value
    } yield {
      _: NodeSeq =>
        // TODO remove hardcoding
        Script(plotToJs(model.get, Full("1"), kindName, indName, depName, Full((indMin, indMax))))
    }
  }

  val model = Select(Val(Seq(Point)), (t: BaseMapper) => t.dbName)

  val kind = Select(Val(Seq("blank", "group", "time")))

  val ind = Select(kind.selectedItem.map(s => getInd(model.selectedItem.value.get, s)))

  val dep = Select(kind.selectedItem.map(s => getDep(model.selectedItem.value.get, s)))

  import code.helper.Formatter._

  val start = TextInput()
  start.value() = format(now.minusDays(1))

  val end = TextInput()
  end.value() = format(now)

  val results = Cell(plot)

  def render =
    "#model" #> model &
      "#plotKind" #> kind &
      "#ind" #> ind &
      "#dep" #> dep &
      "#start" #> start &
      "#end" #> end &
      "#results" #> results
}
