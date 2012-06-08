package code.model

import net.liftweb.mapper._
import code.service.Service
import code.helper.{ToDate, ForeignKeyField, DateField, Identity}

/**
 * The singleton that has methods for accessing the database
 */
object Location extends Location with LongKeyedMetaMapper[Location]
with CRUDify[Long, Location] with Service[Location] {
  override def dbTableName = "Locations"

  // define the DB table name
  override def fieldOrder = List(device, date, latitude, longitude)

  def expose = ("device", Identity) ::("date", ToDate) ::
    ("latitude", Identity) ::("longitude", Identity) :: Nil
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