package code.model

import net.liftweb.mapper.LongKeyedMetaMapper
import net.liftweb.mapper.CRUDify
import code.service.Service
import net.liftweb.mapper.LongKeyedMapper
import net.liftweb.mapper.IdPK
import code.helper._

/**
 * The singleton that has methods for accessing the database
 */
case object Notification extends Notification with LongKeyedMetaMapper[Notification]
  with CRUDify[Long, Notification] with Service[Notification] {
  override def dbTableName = "notifications" // define the DB table name
  override def fieldOrder = List(message)

  def expose = ("message", ToLong) :: Nil
}

/**
 * An O-R mapped class
 */
class Notification extends LongKeyedMapper[Notification] with IdPK {
  def getSingleton = Notification // reference to the companion object above

  object message extends ForeignKeyField(this, Message)
}
