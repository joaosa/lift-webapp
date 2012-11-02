package code.model

import code.service.DomainService
import net.liftweb.mapper._
import code.helper.Transformer.Identity

/**
 * The singleton that has methods for accessing the database
 */
object Raw extends Raw with LongKeyedMetaMapper[Raw]
with CRUDify[Long, Raw] with DomainService[Raw] {
  override def dbTableName = "raw"

  // define the DB table name
  override def fieldOrder = List(data)

  def expose = Seq((data, Identity))
}

/**
 * An O-R mapped class
 */
class Raw extends LongKeyedMapper[Raw] with IdPK {
  // reference to the companion object above
  def getSingleton = Raw

  object data extends MappedText(this)

}
