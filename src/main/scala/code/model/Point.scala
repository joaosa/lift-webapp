package code.model

import net.liftweb.mapper.{
  LongKeyedMetaMapper,
  LongKeyedMapper,
  IdPK,
  CRUDify,
  MappedDouble
}
import code.service.Service
import code.helper._

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

  object data extends ForeignKeyField(this, Data, Data.name)

  object independent extends MappedDouble(this)

  object dependent extends MappedDouble(this)
}
