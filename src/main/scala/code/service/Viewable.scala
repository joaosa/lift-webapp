package code.service

import code.helper.ForeignKeyField
import net.liftweb.common.Full
import net.liftweb.util.FieldError
import net.liftweb.mapper.{IdPK, BaseMapper}

case class View(name: String, items: List[(String, String)])

trait Viewable[T] {
  def toView(t: T): View

  def list(t: List[T]): List[View] = t.map(toView)
}

// TODO abstract "BaseMapper with IdPK"
object Viewer {

  def toListView[T: Viewable](t: List[T]) = implicitly[Viewable[T]].list(t)

  def toView[T: Viewable](t: T) = implicitly[Viewable[T]].toView(t)

  // TODO recursion & create a flag to enable it
  implicit object BaseMapper extends Viewable[BaseMapper with IdPK] {
    def toView(item: BaseMapper with IdPK): View = {
      val fields = item.allFields.toList.flatMap {
        case f: ForeignKeyField[_, _] => (f.name, f.toString()) :: Nil//FKView(f)
        case f => (f.name, f.asHtml.toString()) :: Nil
      }
      View(item.dbName.toLowerCase, fields ::: ("id", item.id.is.toString) :: Nil)
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

  implicit object Error extends Viewable[FieldError] {
    def toView(item: FieldError) = View("error", (item.field.toString, item.msg.toString()) :: Nil)

  }

  // TODO only supports one FieldError, fix by replacing Viewable[BaseMapper] with Viewable[BaseField]
  implicit object FieldErrorOrBaseMapper extends Viewable[Either[List[FieldError], BaseMapper with IdPK]] {
    def toView(t: Either[List[FieldError], BaseMapper with IdPK]): View = {
      t match {
        case Left(failure) => implicitly[Viewable[FieldError]].toView(failure.head)
        case Right(result) => implicitly[Viewable[BaseMapper with IdPK]].toView(result)
      }
    }
  }

}
