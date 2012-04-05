package code.service

import net.liftweb.mapper.{KeyedMapper, KeyedMetaMapper}
import net.liftweb.common.{Empty, Box, Full}
import code.helper._

// TODO TypeClassify
/*object CRUD {
  implicit def messageCRUD = new CRUD[Message] {
    def expose = ("kind", Identity) :: ("date", Now) :: ("source", ByUserName) ::
    ("destination", ByUserName) :: ("content", Identity) :: Nil
  }
}

trait CRUD[CRUDType <: KeyedMapper[_, CRUDType]] {
  def expose: List[(String, Transform)]
}  */

trait CRUDifiable[CRUDType <: KeyedMapper[_, CRUDType]] {
  self: CRUDType with KeyedMetaMapper[_, CRUDType] =>

  def expose: List[(String, Transform)]

  def FKSetup(f: ForeignKeyField[_, _], data: Any) {

  }

  def setup[OwnerType <: KeyedMapper[_, OwnerType]]
  (b: Box[KeyedMapper[_, OwnerType]],
   extractor: (Any, String) => Box[String], data: Any): Box[KeyedMapper[_, _]] = {
    for {
      item <- b
      values <- Full(expose map {
        case (field, transform) => extractor(data, field)
      })
    } yield {
      (expose zip values) map {
        case ((fieldName, transform), value) =>
          item.fieldByName[Any](fieldName) match {
            case Full(field) => field.set_?(transform(value))
              /*field match {
                case fk: ForeignKeyField[Any, OwnerType] => FKSetup(fk, data)
                case _ => field.set_?(transform(value))
              }*/
            case _ => Empty
          }
      }
      item
    }
  }

  def list: Box[List[KeyedMapper[_, _]]] = Full(findAll())

  def create(extractor: (Any, String) => Box[String], data: Any): Box[KeyedMapper[_, _]] = {
    setup(Full(create), extractor, data)
  }

  def read(id: String): Box[KeyedMapper[_, _]] =
    for {
      item <- find(id)
    } yield item

  def update(id: String, extractor: (Any, String) => Box[String],
             data: Any): Box[KeyedMapper[_, _]] = {
    setup(find(id), extractor, data)
  }

  def delete(id: String): Box[KeyedMapper[_, _]] = {
    for {
      item <- find(id)
    } yield {
      delete_!(item)
      item
    }
  }
}
