package code.model

import net.liftweb.mapper._
import code.helper._
import code.service.Service

/**
 * The singleton that has methods for accessing the database
 */
object Device extends Device with LongKeyedMetaMapper[Device]
  with CRUDify[Long, Device] with Service[Device] {
  override def dbTableName = "devices" // define the DB table name
  override def fieldOrder = List(user, name, address)

  def expose = ("user", ByUserName) :: ("name", Identity) ::
    ("address", Identity) :: ("port", ToInt) :: Nil
}

/**
 * An O-R mapped class
 */
class Device extends LongKeyedMapper[Device] with IdPK {
  def getSingleton = Device // reference to the companion object above

  object user extends ForeignKeyField(this, User)

  object name extends MappedString(this, 15)

  object address extends MappedText(this) // IPV4

  object port extends MappedInt(this)

  object online extends MappedBoolean(this)

  object location extends ForeignKeyField(this, Location)

}
