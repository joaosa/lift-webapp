package code.model

import net.liftweb.mapper.{
LongKeyedMetaMapper,
LongKeyedMapper,
IdPK,
CRUDify
}
import code.helper._
import code.service.DomainService
import net.liftweb.mapper.MappedText

/**
 * The singleton that has methods for accessing the database
 */
object Message extends Message with LongKeyedMetaMapper[Message]
with CRUDify[Long, Message] with DomainService[Message] {
  override def dbTableName = "messages"

  // define the DB table name
  override def fieldOrder = List(kind, date, source, destination, content)

  def expose = ("kind", Identity) ::("date", Now) ::("source", ByUserName) ::
    ("destination", ByUserName) ::("content", Identity) :: Nil
}

/**
 * An O-R mapped "Message" class
 */
class Message extends LongKeyedMapper[Message] with IdPK {
  def getSingleton = Message // reference to the companion object above

  def show = "%s %s" format(kind.asHtml, date.asHtml)

  object kind extends ValueListField(this,
    List("Notification"))

  object date extends DateField(this)

  object source extends ForeignKeyField(this, User)

  object destination extends ForeignKeyField(this, User)

  object content extends MappedText(this)

}
