package code.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.common.Box
import code.service.DomainService
import code.helper.Transformer.{Identity, ByEmail}
import code.helper.{Formattable, DateField, ForeignKeyField, ValueListField}
import net.liftweb.common.Full


/**
 * The singleton that has methods for accessing the database
 */
object Data extends Data with LongKeyedMetaMapper[Data]
with CRUDify[Long, Data] with DomainService[Data] {
  override def dbTableName = "data"

  // define the DB table name
  override def fieldOrder = List(kind, user, date)

  def expose = Seq((kind, Identity), (user, ByEmail),
    (date, Identity))

  // TODO cannot filter with ranges if there is no restriction on data
  def pointsInRange(dataId: Box[String], range: Box[(String, String)]) = {
    dataId match {
      case Full(id) =>
        (Data.find(id), range) match {
          case (Full(d), Full(r)) => d.pointsInRange(r)
          case _ => Nil
        }
      case _ => findAll()
    }
  }
}

/**
 * An O-R mapped class
 */
class Data extends LongKeyedMapper[Data] with IdPK {
  def getSingleton = Data // reference to the companion object above

  def show: String = "%s of %s on %s" format(kind, user, date)

  object kind extends ValueListField(this, List("UC", "FHR", "RAW"))

  object user extends ForeignKeyField(this, User)

  object date extends DateField(this)

  def raws = Raw.findAll(By(Raw.data, id.is))

  def points = Point.findAll(By(Point.data, id.is))

  def pointsInRange(min: Date, max: Date): List[Point] =
    Point.findAll(By(Point.data, id.is),
      By_>=(Point.date, min),
      By_<=(Point.date, max))

  def pointsInRange(range: (String, String)): List[Point] = {
    import code.helper.Formatter._
    val min = implicitly[Formattable[Date]].parse(range._1)
    val max = implicitly[Formattable[Date]].parse(range._2)
    (min, max) match {
      case (Full(x1), Full(x2)) => pointsInRange(x1, x2)
      case _ => Nil
    }
  }
}
