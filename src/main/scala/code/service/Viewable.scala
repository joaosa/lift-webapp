package code.service

import code.helper.ForeignKeyField
import net.liftweb.common.Full
import net.liftweb.util.{BaseField, FieldError}
import net.liftweb.mapper.{IdPK, BaseMapper}

case class View(name: String, items: List[(String, String)])

trait Viewable[-T] {
  def toView(t: T): View

  def list(t: List[T]): List[View] = t.map(toView)
}

// TODO abstract "BaseMapper with IdPK"
object Viewer {

  implicit def toListView[T: Viewable](t: List[T]) = implicitly[Viewable[T]].list(t)

  implicit def toView[T: Viewable](t: T) = implicitly[Viewable[T]].toView(t)

  // TODO recursion & create a flag to enable it
  implicit object BaseMapperView extends Viewable[BaseMapper with IdPK] {
    def toView(item: BaseMapper with IdPK): View = {
      val fields = item.allFields.toList.flatMap {
        case f: ForeignKeyField[_, _] => FKView(f) :::(f.name, f.toString()) :: Nil
        case f => (f.name, f.asHtml.toString()) :: Nil
      }
      View(item.dbName.toLowerCase, fields :::("id", item.id.is.toString) :: Nil)
    }

    def FKView(fk: ForeignKeyField[_, _]): List[(String, String)] = {
      fk.foreign match {
        case Full(m: BaseMapper) => m.allFields.flatMap {
          case f => (fk.name + "." + f.name, f.asHtml.toString()) :: Nil
        }.toList
        case _ => Nil
      }
    }

  }

  implicit object ErrorView extends Viewable[FieldError] {
    def toView(t: FieldError) = View("error", (t.field.toString, t.msg.toString()) :: Nil)

  }

  // TODO only supports one FieldError, fix by replacing Viewable[BaseMapper] with Viewable[BaseField]
  implicit object FieldErrorOrBaseMapperView extends Viewable[Either[List[FieldError], BaseMapper with IdPK]] {
    def toView(t: Either[List[FieldError], BaseMapper with IdPK]): View = {
      t match {
        case Left(failure) => implicitly[Viewable[FieldError]].toView(failure.head)
        case Right(result) => implicitly[Viewable[BaseMapper with IdPK]].toView(result)
      }
    }
  }

  implicit object MessageView extends Viewable[Message] {
    def toView(t: Message) = View("message", t.content)
  }

  implicit object FieldView extends Viewable[(BaseMapper, BaseField)] {
    def toView(t: (BaseMapper, BaseField)) =
      View(t._1.dbName.toLowerCase,
      (t._2.name, t._2.asHtml.toString()) :: Nil)
  }

}
