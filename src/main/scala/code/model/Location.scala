package code.model

import net.liftweb.mapper._
import code.service.Service
import code.helper.Identity

/**
 * The singleton that has methods for accessing the database
 */
object Location extends Location with LongKeyedMetaMapper[Location]
with CRUDify[Long, Location] with Service[Location] {
  override def dbTableName = "Locations"

  // define the DB table name
  override def fieldOrder = List(latitude, longitude)

  def expose = ("latitude", Identity) ::
    ("longitude", Identity) :: Nil
}

/**
 * An O-R mapped class
 */
class Location extends LongKeyedMapper[Location] with IdPK {
  // reference to the companion object above
  def getSingleton = Location

  def show = (latitude.asHtml, longitude.asHtml).toString()

  // TODO include gp coordinate validation
  // TODO constraint: pair (la, lo) must be unique
  object latitude extends MappedString(this, 15)

  object longitude extends MappedString(this, 15)

}
