package code.service

import net.liftweb.common.{Empty, Box, Full}
import code.helper._
import net.liftweb.util.FieldError
import net.liftweb.mapper.{BaseMapper, Mapper, KeyedMapper, KeyedMetaMapper}
import net.liftweb.json._
import xml.Elem

trait Extractor[T] {
  def extract(t: T, field: String): Box[String]
}

object Extractor {

  implicit object JsonExtractor extends Extractor[JValue] {
    def extract(t: JValue, field: String): Box[String] = {
      Box(for (JString(str) <- t \ field) yield str)
    }
  }

  implicit object XmlExtractor extends Extractor[Elem] {
    def extract(t: Elem, field: String): Box[String] = {
      Full((t \ field).text)
    }
  }

}


trait CRUDifiable[CRUDType <: KeyedMapper[_, CRUDType]] {
  self: CRUDType with KeyedMetaMapper[_, CRUDType] =>

  def expose: List[(String, Transform)]

  // TODO finish method
  def FKSetup(f: ForeignKeyField[_, _], data: Any) {

  }

  def extractValues[T: Extractor](t: T): List[Box[String]] = expose map {
    case (field, transform) => implicitly[Extractor[T]].extract(t, field)
  }

  def setup[OwnerType <: KeyedMapper[_, OwnerType]]
  (b: Box[KeyedMapper[_, OwnerType]], values: List[Box[String]]): Box[Mapper[_]] = {
    for {
      item <- b
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

  def validate(b: Box[Mapper[_]]): Either[List[FieldError], BaseMapper] = b match {
    case Full(item) => {
      item.validate match {
        case Nil =>
          item.save()
          Right(item.asInstanceOf[BaseMapper])
        case xs => Left(xs)
      }
    }
    case _ => Left(Nil)
  }

  def create[T: Extractor](t: T): Either[List[FieldError], BaseMapper] = {
    validate(setup(Full(create), extractValues(t)))
  }

  /*def createList(data: Any): Box[List[BaseMapper]] = {

  }   */

  def read(id: String): Box[BaseMapper] =
    for {
      item <- find(id)
    } yield item

  def readAll: Box[List[BaseMapper]] = Full(findAll())

  def update[T: Extractor](id: String, t: T): Either[List[FieldError], BaseMapper] = {
    validate(setup(find(id), extractValues(t)))
  }

  def delete(id: String): Box[BaseMapper] = {
    for {
      item <- find(id)
    } yield {
      delete_!(item)
      item
    }
  }

}
