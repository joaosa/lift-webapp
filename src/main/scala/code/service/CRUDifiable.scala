package code.service

import net.liftweb.common.{Empty, Box, Full}
import code.helper._
import net.liftweb.util.{BaseField, FieldError}
import net.liftweb.mapper._

// TODO abstract "BaseMapper with IdPK"
trait CRUDifiable[CRUDType <: KeyedMapper[_, CRUDType]] {
  self: KeyedMetaMapper[_, CRUDType] =>

  def expose: List[(String, Transform)]

  // TODO finish method
  def FKSetup(f: ForeignKeyField[_, _], data: Any) {

  }

  def transformValues[T: Extractable](t: T): List[(String, Box[Any])] = {
    expose.map(_._1) zip (expose map {
      case (field, transform) => transform(Extractor.extractField(t, field))
    })
  }

  def setup(b: Box[CRUDType], kv: List[(String, Box[Any])]): Box[Mapper[_]] = {
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

  def validate(b: Box[Mapper[_]]): Either[List[FieldError], BaseMapper with IdPK] = b match {
    case Full(item) => {
      item.validate match {
        case Nil =>
          item.save()
          Right(item.asInstanceOf[BaseMapper with IdPK])
        case xs => Left(xs)
      }
    }
    case _ => Left(Nil)
  }

  def create[T: Extractable](t: T): Either[List[FieldError], BaseMapper with IdPK] = {
    validate(setup(Full(create), transformValues(t)))
  }

  def createList[T: Extractable](t: T, modelName: String): List[Either[List[FieldError], BaseMapper with IdPK]] = {
    Extractor.extractModel(t, modelName).map(create(_))
  }

  def read(id: String): Box[BaseMapper with IdPK] =
    for {
      item <- find(id)
    } yield item.asInstanceOf[BaseMapper with IdPK]

  def readAll: Box[List[BaseMapper with IdPK]] = Full(findAll().map(_.asInstanceOf[BaseMapper with IdPK]))

  def readField(id: String, fieldName: String): Box[(BaseMapper, BaseField)] =
    for {
      item <- find(id)
      fields <- Box !! item.allFields.map(f => (f.name, f)).toMap
      field <- fields.get(fieldName)
    } yield (item, field)


  def update[T: Extractable](id: String, t: T): Either[List[FieldError], BaseMapper with IdPK] = {
    validate(setup(find(id), transformValues(t)))
  }

  def delete(id: String): Box[BaseMapper with IdPK] = {
    for {
      item <- find(id)
    } yield {
      delete_!(item)
      item.asInstanceOf[BaseMapper with IdPK]
    }
  }

}
