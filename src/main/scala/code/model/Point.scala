package code.model

import code.service.DomainService
import net.liftweb.mapper._
import code.helper.Transformer.{ToDouble, ToDate, ToLong}
import code.helper.{DateField, ForeignKeyField}

/**
 * The singleton that has methods for accessing the database
 */
object Point extends Point with LongKeyedMetaMapper[Point]
with CRUDify[Long, Point] with DomainService[Point] {
  override def dbTableName = "points"

  // define the DB table name
  override def fieldOrder = List(data, date, independent, dependent)

  def expose = Seq((data, ToLong), (date, ToDate),
    (independent, ToDouble), (dependent, ToDouble))
}

/**
 * An O-R mapped class
 */
class Point extends LongKeyedMapper[Point] with IdPK {
  // reference to the companion object above
  def getSingleton = Point

  object data extends ForeignKeyField(this, Data)

  object date extends DateField(this)

  object independent extends MappedDouble(this) {
    // TODO to enable when client side implementation handles this
    /*override def validations = validateUniqueness _ :: Nil
    def validateUniqueness(v: Double) = {
      Point.findAll(By(Point.data, data), By(Point.independent, v)) match {
        case Nil => Nil
        case _ => List(FieldError(this, "Value must be unique"))
      }
    }*/
  }

  object dependent extends MappedDouble(this)

}
