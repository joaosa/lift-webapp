package code.model

import net.liftweb.mapper._
import code.service.DomainService
import net.liftweb.http.S
import net.liftweb.common.{Empty, Full, Box}
import code.helper.Transformer.{ByEmail, Identity}
import code.helper.{ForeignKeyField, ValueListField}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with LongKeyedMetaMapper[User]
with CRUDify[Long, User] with DomainService[User] {

  // define the DB table name
  override def dbTableName = "users"

  // define the order fields will appear in forms and output
  override def fieldOrder = List(username, email, password, role)

  def expose = Seq((username, Identity), (email, Identity),
    (password, Identity), (role, Identity))

  def findByName(name: String): Box[User] = find(By(User.username, name))

  def findByEmail(email: String): Box[User] = find(By(User.email, email))

  def authenticate(email: String, password: String): Box[User] = {
    User.findByEmail(email) match {
      case Full(user) if user.password.match_?(password) => Full(user)
      case _ => Empty
    }
  }

  def devices(u: User) = Device.findAll(By(Device.user, u.id.is))

  def devicesOnline(u: User) = devices(u).filter(_.online.is)
}

/**
 * An O-R mapped class
 */
class User extends LongKeyedMapper[User] with IdPK with ManyToMany {
  // reference to the companion object above
  def getSingleton = User

  def show = email.asHtml.toString()

  object username extends MappedString(this, 32)

  object email extends MappedEmail(this, 48) {
    override def validations =
      valUnique(S.?("unique.email.address")) _ :: super.validations
  }

  object password extends MappedPassword(this)

  object role extends ValueListField(this, List("user", "doc", "admin")) {
    override def defaultValue = "user"
  }

  object users extends MappedManyToMany(UserRelation,
    UserRelation.source, UserRelation.destination, User)

  // TODO: support timezone and locale (look into MegaProtoUser)

  def relations = UserRelation.findAll(By(UserRelation.source, id.is))

  def data = Data.findAll(By(Data.user, id.is))

  def isDeviceOwner(d: Device): Boolean =
    Device.findAll(By(Device.user, id.is)).contains(d)

  def isDeviceOwnerByID(deviceID: String): Boolean =
    Device.findByID(deviceID) match {
      case Full(d) => isDeviceOwner(d)
      case _ => false
    }
}

object UserRelation extends UserRelation
with LongKeyedMetaMapper[UserRelation] with CRUDify[Long, UserRelation]
with DomainService[UserRelation] {
  // define the DB table name
  override def dbTableName = "userRelations"

  // define the order fields will appear in forms and output
  override def fieldOrder = List(source, destination, kind)

  def expose = (source, ByEmail) ::(destination, ByEmail) ::
    (kind, Identity) :: Nil
}

class UserRelation extends LongKeyedMapper[UserRelation] with IdPK {
  // reference to the companion object above
  def getSingleton = UserRelation

  object source extends ForeignKeyField(this, User)

  object destination extends ForeignKeyField(this, User)

  object kind extends ValueListField(this, List("doctor", "patient", "friend"))

}
