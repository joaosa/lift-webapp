package code.model

import code.service.Service
import code.helper._
import net.liftweb.mapper._
import net.liftweb.util.FieldError

/**
 * The singleton that has methods for accessing the database
 */
object Point extends Point with LongKeyedMetaMapper[Point]
  with CRUDify[Long, Point] with Service[Point] {
  override def dbTableName = "points" // define the DB table name
  override def fieldOrder = List(data, independent, dependent)

  def expose = ("data", ToLong) :: ("independent", ToDouble) :: 
  ("dependent", ToDouble) :: Nil
}

/**
 * An O-R mapped class
 */
class Point extends LongKeyedMapper[Point] with IdPK {
  def getSingleton = Point // reference to the companion object above

  object data extends ForeignKeyField(this, Data)

  object independent extends MappedDouble(this) {
    override def validations = validateUniqueness _ :: Nil
    def validateUniqueness(v: Double) = {
      Point.findAll(By(Point.data, data)) match {
        case Nil => Nil
        case _ => List(FieldError(this, "Value must be unique"))
      }
    }
  }

  object dependent extends MappedDouble(this)
}
