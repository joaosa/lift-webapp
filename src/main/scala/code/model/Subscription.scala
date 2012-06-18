package code.model

import net.liftweb.mapper._
import net.liftweb.common.Full
import code.helper._
import code.service.DomainService

/**
 * The singleton that has methods for accessing the database
 */
object Subscription extends Subscription with LongKeyedMetaMapper[Subscription]
with CRUDify[Long, Subscription] with DomainService[Subscription] {
  override def dbTableName = "subscriptions"

  // define the DB table name
  override def fieldOrder = List(user)

  def expose = ("kind" -> Identity) :: ("user" -> ByEmail) :: Nil

  def findByEmail(email: String): List[Subscription] =
    User.find(Like(User.email, email)) match {
      case Full(u) =>
        Subscription.findAll(By(Subscription.user, u.id.is)).toList
      case _ => Nil
    }
}

/**
 * An O-R mapped class
 */
class Subscription extends LongKeyedMapper[Subscription] with IdPK {
  // reference to the companion object above
  def getSingleton = Subscription

  object date extends DateField(this)

  object kind extends ValueListField(this, List("Alert"))

  object user extends ForeignKeyField(this, User)

}
