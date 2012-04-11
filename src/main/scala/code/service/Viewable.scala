package code.service

import code.helper.ForeignKeyField
import net.liftweb.common.Full
import net.liftweb.util.FieldError
import net.liftweb.mapper.{Mapper, KeyedMapper}

case class View(name: String, items: List[(String, String)])

trait Viewable[T] {
  def toView(t: T): View
  def list(t: List[T]): List[View] = t.map(toView)
}

object Viewable {

  // TODO recursion
  implicit object Mapper extends Viewable[Mapper[_]] {
    def toView(item: Mapper[_]): View = {
      val fields = item.allFields.map {
        case f: ForeignKeyField[_, _] => FKView(f)
        case f => (f.name, f.asHtml.toString()) :: Nil
      }.toList.flatten
      View(item.dbName.toLowerCase, fields)
    }

    def FKView(fk: ForeignKeyField[_, _]): List[(String, String)] = {
      fk.foreign match {
        case Full(km: KeyedMapper[_, _]) => km.allFields.map {
          case f => (fk.name + "." + f.name, f.asHtml.toString()) :: Nil
        }.toList.flatten
        case _ => Nil
      }
    }

  }

  implicit object Error extends Viewable[FieldError] {
    def toView(item: FieldError) = View("error", (item.field.toString, item.msg.toString()) :: Nil)

  }

}
