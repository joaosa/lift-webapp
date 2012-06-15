package code.model

import net.liftweb.mapper._
import code.helper._
import code.service.DomainService
import net.liftweb.http.S
import net.liftweb.common.{Empty, Full, Box}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with LongKeyedMetaMapper[User]
with CRUDify[Long, User] with DomainService[User] {

  override def dbTableName = "users"

  // define the DB table name
  // define the order fields will appear in forms and output
  override def fieldOrder = List(username, email, password, role)

  def expose = ("username", Identity) ::("email", Identity) ::
    ("password", Identity) ::("role", Identity) :: Nil

  def findByName(name: String): Box[User] = find(By(User.username, name))

  def findByEmail(email: String): Box[User] = find(By(User.email, email))

  def authenticate(email: String, password: String): Box[User] = {
    User.findByEmail(email) match {
      case Full(user) if user.password.match_?(password) => Full(user)
      case _ => Empty
    }
  }

  def devices(u: User) = Device.findAll(By(Device.user, u.id.is))

  def data(u: User) = Data.find(By(Data.user, u.id.is))

  def devicesOnline(u: User) = devices(u).filter(_.online.is)
}

/**
 * An O-R mapped class
 */
class User extends LongKeyedMapper[User] with IdPK {
  // reference to the companion object above
  def getSingleton = User

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

  def data = Data.findAll(By(Data.user, id.is))

  // TODO: support timezone and locale (look into MegaProtoUser)

  def isDeviceOwner(d: Device): Boolean =
    Device.findAll(By(Device.user, id.is)).contains(d)

  def isDeviceOwnerByID(deviceID: String): Boolean =
    Device.findByID(deviceID) match {
      case Full(d) => isDeviceOwner(d)
      case _ => false
    }
}
