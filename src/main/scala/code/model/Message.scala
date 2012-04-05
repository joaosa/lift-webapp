package code.model

import net.liftweb.mapper.{
  LongKeyedMetaMapper,
  LongKeyedMapper,
  IdPK,
  CRUDify
}
import code.helper._
import code.service.Service
import net.liftweb.mapper.MappedText

/**
 * The singleton that has methods for accessing the database
 */
object Message extends Message with LongKeyedMetaMapper[Message]
  with CRUDify[Long, Message] with Service[Message] {
  override def dbTableName = "messages" // define the DB table name
  override def fieldOrder = List(kind, date, source, destination, content)

  def expose = ("kind", Identity) :: ("date", Now) :: ("source", ByUserName) :: 
  ("destination", ByUserName) :: ("content", Identity) :: Nil

  def name(e: Message) = "%s %s" format (e.kind.asHtml, e.date.asHtml)
}

/**
 * An O-R mapped "Message" class
 */
class Message extends LongKeyedMapper[Message] with IdPK {
  def getSingleton = Message // reference to the companion object above

  object kind extends ValueListField(this, List("Ping", "Data", "Alert"))

  object date extends DateField(this)

  object source extends ForeignKeyField(this, User, User.name)

  object destination extends ForeignKeyField(this, User, User.name)

  object content extends MappedText(this)
}
