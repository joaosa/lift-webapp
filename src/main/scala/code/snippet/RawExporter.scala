package code.snippet

import reactive.Observing
import net.liftweb.util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.html.Button
import net.liftweb.http.S
import code.service.{Filer => FileService}

class RawExporter extends Observing {

  val trigger = Button("Export") {
    S.notice(FileService.toFile(dataID.value.value, "RECORD.BIN"))
  }

  val dataID = TextInput()
  dataID.value updateOn trigger.click

  def render = {
    "#dataID" #> dataID &
      "#trigger" #> trigger
  }
}
