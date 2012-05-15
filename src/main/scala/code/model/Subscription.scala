package code.model

import net.liftweb.mapper.{
  LongKeyedMetaMapper,
  LongKeyedMapper,
  IdPK,
  CRUDify
}
import code.helper._
import code.service.Service

/**
 * The singleton that has methods for accessing the database
 */
object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]
  with CRUDify[Long, Subscription] with Service[Subscription] {
  override def dbTableName = "subscriptions" // define the DB table name
  override def fieldOrder = List(user)

  def expose = ("kind" -> Identity) :: ("user" -> ByUserName) :: Nil
}

/**
 * An O-R mapped class
 */
class Subscription extends LongKeyedMapper[Subscription] with IdPK {
  def getSingleton = Subscription // reference to the companion object above

  object date extends DateField(this)

  object kind extends ValueListField(this, List("Alert"))

  object user extends ForeignKeyField(this, User)
}
