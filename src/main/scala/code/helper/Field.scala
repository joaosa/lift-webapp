package code.helper

import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.util.Helpers.now
import net.liftweb.common.Full
import xml.Text

abstract class ForeignKeyField[T <: KeyedMapper[_, T], O <: KeyedMapper[Long, O] with IdPK {def show: String}](theOwner: T, _foreignMeta: => KeyedMetaMapper[Long, O])
  extends MappedLongForeignKey(theOwner , _foreignMeta) {
  override def validSelectValues = {
    Full(for (e <- foreignMeta.getSingleton.findAll()) yield (e.id.is, e.show))
  }
  override def asHtml = Text(foreign.map(e => e.toString()) openOr "None")
}

abstract class DateField[T <: Mapper[T]](theOwner: T) extends MappedDateTime(theOwner) {
  import code.helper.Formatter._
  override def asHtml = Text(Formatter.format(is))
  override def defaultValue = now
}

abstract class DomainField[T <: Mapper[T]](theOwner: T, domainRestriction: (Double) => Boolean) extends MappedDouble(theOwner) {
  override def validations = validateDomain _ :: Nil
  def validateDomain(value: Double) = {
    value match {
      case x if domainRestriction(value) => Nil
      case _ => List(FieldError(this, "Invalid value"))
    }
  }
}

abstract class ProbabilityField[T <: Mapper[T]](theOwner: T)
  extends DomainField(theOwner, x => 0 <= x && x <= 1)

abstract class ValueListField[T <: Mapper[T]](theOwner: T, values: List[String]) extends MappedText(theOwner) {
  override def validations = validateKind _ :: Nil
  def validateKind(kind: String) = {
    kind match {
      case x if values.contains(kind) => Nil
      case _ => List(FieldError(this, "Invalid value"))
    }
  }
}
