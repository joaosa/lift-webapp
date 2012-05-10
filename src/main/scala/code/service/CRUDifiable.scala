package code.service

import net.liftweb.common.{Empty, Box, Full}
import code.helper._
import net.liftweb.util.FieldError
import net.liftweb.mapper.{BaseMapper, Mapper, KeyedMapper, KeyedMetaMapper}
import net.liftweb.json._
import xml.Elem

trait Extractor[T] {
  def extractField(t: T, field: String): Box[String]
  def extractModel(t: T, modelName: String): List[T]
}

object Extractor {

  implicit object JsonExtractor extends Extractor[JValue] {
    def extractField(t: JValue, field: String): Box[String] = {
      Box(for (JString(s) <- t \\ field) yield s)
    }
    def extractModel(t: JValue, modelName: String): List[JValue] = {
      (for (JArray(m) <- t \\ modelName) yield m).flatten
    }
  }

  implicit object XmlExtractor extends Extractor[Elem] {
    def extractField(t: Elem, field: String): Box[String] = {
      Full((t \ field).text)
    }
    def extractModel(t: Elem, modelName: String): List[Elem] = {
      // TODO: implement
      List()
    }
  }

}


trait CRUDifiable[CRUDType <: KeyedMapper[_, CRUDType]] {
  self: CRUDType with KeyedMetaMapper[_, CRUDType] =>

  def expose: List[(String, Transform)]

  // TODO finish method
  def FKSetup(f: ForeignKeyField[_, _], data: Any) {

  }

  def transformValues[T: Extractor](t: T): List[(String, Box[Any])] = {
    expose.map(_._1) zip (expose map {
      case (field, transform) => transform(implicitly[Extractor[T]].extractField(t, field))
    })
  }

  def setup[OwnerType <: KeyedMapper[_, OwnerType]]
  (b: Box[KeyedMapper[_, OwnerType]], kv: List[(String, Box[Any])]): Box[Mapper[_]] = {
    for {
      item <- b
    } yield {
      kv map {
        case (fieldName, value) =>
          item.fieldByName[Any](fieldName) match {
            case Full(field) => field.set_?(value)
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
    validate(setup(Full(create), transformValues(t)))
  }

  def createList[T: Extractor](t: T, modelName: String): List[Either[List[FieldError], BaseMapper]] = {
    implicitly[Extractor[T]].extractModel(t, modelName).map(create(_))
  }

  def read(id: String): Box[BaseMapper] =
    for {
      item <- find(id)
    } yield item

  def readAll: Box[List[BaseMapper]] = Full(findAll())

  def update[T: Extractor](id: String, t: T): Either[List[FieldError], BaseMapper] = {
    validate(setup(find(id), transformValues(t)))
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
