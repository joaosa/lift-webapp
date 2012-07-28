package code.model

import net.liftweb.mapper._
import code.service.DomainService
import code.helper.Transformer.{Identity, ToDate, ToLong}
import code.helper.{DateField, ForeignKeyField}

/**
 * The singleton that has methods for accessing the database
 */
object Location extends Location with LongKeyedMetaMapper[Location]
with CRUDify[Long, Location] with DomainService[Location] {
  override def dbTableName = "locations"

  // define the DB table name
  override def fieldOrder = List(device, date, latitude, longitude)

  def expose = Seq((device, ToLong), (date, ToDate),
    (latitude, Identity), (longitude, Identity))
}

/**
 * An O-R mapped class
 */
class Location extends LongKeyedMapper[Location] with IdPK {
  // reference to the companion object above
  def getSingleton = Location

  object device extends ForeignKeyField(this, Device)

  object date extends DateField(this)

  // TODO include gp coordinate validation
  object latitude extends MappedString(this, 15)

  object longitude extends MappedString(this, 15)

}
