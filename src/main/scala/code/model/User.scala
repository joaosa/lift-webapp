package code.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http.S
import code.service.Service
import code.helper._
import net.liftweb.http.auth.userRoles
import net.liftweb.http.auth.AuthRole

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with LongKeyedMetaMapper[User]
  with CRUDify[Long, User] with Service[User] {

  override def dbTableName = "users" // define the DB table name
  // define the order fields will appear in forms and output
  override def fieldOrder = List(username, email, password, role)

  def expose = ("username", Identity) :: ("email", Identity) ::
    ("password", Identity) :: ("role", Identity) :: Nil

  // find by first name
  def findByName(name: String): Box[User] = find(Like(username, name))

  def getUsername(firstName: String, lastName: String) =
    "%s %s" format (firstName, lastName)

  def authenticate(email: String, password: String): Box[User] = {
    User.find(By(User.email, email)) match {
      case Full(user) if user.password.match_?(password) => Full(user)
      case _ => Empty
    }
  }

  def login(login: String, password: String): Boolean = {
    authenticate(login, password) match {
      case Full(user) =>
        S.setSessionAttribute("user", user.email.is)
        userRoles(AuthRole(user.role.is) :: Nil)
        S.notice("Login successful!")
        S.notice(S.getSessionAttribute("user") openOr "")
        true
      case _ =>
        S.notice("Login failed!")
        false
    }
  }

  def devices(u: User): List[Device] = Device.findAll(By(Device.user, u.id.is))
  def devicesOnline(u: User): List[Device] = devices(u).filter(_.online.is)
}

/**
 * An O-R mapped class
 */
class User extends LongKeyedMapper[User] with IdPK {
  def getSingleton = User // reference to the companion object above

  def show = email.asHtml.toString()

  object username extends MappedString(this, 32)

  object email extends MappedEmail(this, 48) {
    override def validations =
      valUnique(S.??("unique.email.address")) _ :: super.validations
  }

  object password extends MappedPassword(this)

  object role extends ValueListField(this, List("user", "admin")) {
    override def defaultValue = "user"
  }

  // TODO: support timezone and locale (look into MegaProtoUser)
}
