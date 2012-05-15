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
case object Data extends Data with LongKeyedMetaMapper[Data]
  with CRUDify[Long, Data] with Service[Data] {
  override def dbTableName = "data" // define the DB table name
  override def fieldOrder = List(kind, user, date)

  def expose = ("kind", Identity) :: ("user", ToLong) ::
    ("date", Identity) :: Nil
}

/**
 * An O-R mapped class
 */
class Data extends LongKeyedMapper[Data] with IdPK {
  def getSingleton = Data // reference to the companion object above

  def show: String = "%s of %s on %s" format (kind, user, date)

  object kind extends ValueListField(this, List("UC", "FHR"))

  object user extends ForeignKeyField(this, User)

  object date extends DateField(this)
}
