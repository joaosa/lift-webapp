package code.service

import net.liftweb.util.FieldError
import net.liftweb.mapper.KeyedMapper
import code.helper.ForeignKeyField
import scala.xml.{ Elem, Null, TopScope }
import net.liftweb.json.{ Xml, Extraction }

case class View(name: String, pairs: List[(String, String)]) extends Convertable {
  def toXml = Elem(null, name, Null, TopScope,
    Xml.toXml(Extraction.decompose(pairs.toMap)).toList: _*)
}

case class GroupView(items: List[View]) extends Convertable {
  def toXml = <list>{ items.map(_.toXml) }</list>
}

case class ErrorView(items: List[FieldError]) extends Convertable {
  def toXml = <errors>{ items.map(_.msg) }</errors>
}

trait Viewable {
  def FKView(fk: ForeignKeyField[_, _]): List[(String, String)] = {
    fk.foreign match {
      case km: KeyedMapper[_, _] => km.pairs.map {
        case (k, v) => (fk.name + k.capitalize, v)
      }
      case _ => Nil
    }
  }

  implicit def toView(item: KeyedMapper[_, _]): View = {
    val fields = item.allFields.map {
      case f: ForeignKeyField[_, _] => FKView(f)
      case f => (f.name, f.asHtml.toString()) :: Nil
    }.toList.flatten
    View(item.dbName.toLowerCase, fields)
  }

  implicit def toGroupView(items: List[KeyedMapper[_, _]]): GroupView = {
    GroupView(items.map(toView))
  }

  implicit def toErrorView(items: List[FieldError]): ErrorView = {
    ErrorView(items)
  }
}
