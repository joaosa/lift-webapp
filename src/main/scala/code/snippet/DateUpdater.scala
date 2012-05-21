package code.snippet

import scala.xml.Text
import net.liftweb.util._
import Helpers._
import reactive._
import reactive.web.html._
import code.helper.Date.{now, format}

class DateUpdater extends Observing {

  // Create an EventStream that fires timer ticks for up to 10 minutes
  val clockES = new reactive.Timer(0 second, 1 second, { t => this; t > (10 minutes) })
  // Create a signal from the EventStream whose value, until
  // the first tick is received, is 0L
  val clockSig = clockES.hold(0L)

  def render = {
    // replace the contents of the element with id "time" with the date
    "#time *" #> Span(clockSig.map(t => Text(format(now))))
  }
}
