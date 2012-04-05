package code.snippet

import reactive.Observing
import net.liftweb.util
import util.Helpers.strToCssBindPromoter
import reactive.web.html.TextInput
import reactive.web.html.Button
import code.model.User

class Login extends Observing {

  val trigger = Button("Login") {
    User.login(login.value.value,
      password.value.value)
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