package code.model

import net.liftweb.mapper._
import code.helper._
import code.service.DomainService


/**
 * The singleton that has methods for accessing the database
 */
object Device extends Device with LongKeyedMetaMapper[Device]
with CRUDify[Long, Device] with DomainService[Device] {
  override def dbTableName = "devices"

  // define the DB table name
  override def fieldOrder = List(user, name, address, port)

  def expose = ("user", ByEmail) ::("name", Identity) ::
    ("address", Identity) ::("port", ToInt) :: Nil

  def findByID(ID: String) = Device.find(By(Device.authID, ID))

}

/**
 * An O-R mapped class
 */
class Device extends LongKeyedMapper[Device] with IdPK {
  // reference to the companion object above
  def getSingleton = Device

  def show = name.asHtml.toString()

  object user extends ForeignKeyField(this, User)

  // ID used on user+device authentication
  // TODO must be unique
  object authID extends MappedText(this)

  object name extends MappedText(this)

  // TODO validate IPV4/V6 address
  object address extends MappedText(this)

  object port extends MappedInt(this)

  object online extends MappedBoolean(this)

}
