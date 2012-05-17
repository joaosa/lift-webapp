package code.snippet

import reactive.Observing
import net.liftweb.util
import util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.html.Button
import net.liftweb.http.S
import code.service.Service

class Login extends Observing {

  val trigger = Button("Login") {
    if (Service.login(login.value.value,
      password.value.value)) {
      S.notice("Login successful!")
      S.notice(S.getSessionAttribute("user") openOr "")
    }
    else {
      S.notice("Login failed!")
    }
  }

  val login = TextInput()
  login.value updateOn trigger.click

  val password = TextInput()
  password.value updateOn trigger.click

  def render = {
    "#login" #> login &
      "#password" #> password &
      "#trigger" #> trigger
  }
}
