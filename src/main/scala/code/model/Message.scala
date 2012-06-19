package code.model

import code.service.DomainService
import code.helper.Transformer.{ToBoolean, Now, ByEmail, Identity}
import code.helper.{ForeignKeyField, DateField, ValueListField}
import net.liftweb.mapper._

/**
 * The singleton that has methods for accessing the database
 */
object Message extends Message with LongKeyedMetaMapper[Message]
with CRUDify[Long, Message] with DomainService[Message] {
  override def dbTableName = "messages"

  // define the DB table name
  override def fieldOrder = List(kind, date, source, destination, content)

  def expose = ("kind", Identity) ::("date", Now) ::("source", ByEmail) ::
    ("destination", ByEmail) ::("content", Identity) ::
    ("delivered", ToBoolean) :: Nil
}

/**
 * An O-R mapped "Message" class
 */
class Message extends LongKeyedMapper[Message] with IdPK {
  def getSingleton = Message // reference to the companion object above

  def show = "%s %s" format(kind.asHtml, date.asHtml)

  object kind extends ValueListField(this, List("alert"))

  object date extends DateField(this)

  object source extends ForeignKeyField(this, User)

  object destination extends ForeignKeyField(this, User)

  object content extends MappedText(this)

  object delivered extends MappedBoolean(this)

}
