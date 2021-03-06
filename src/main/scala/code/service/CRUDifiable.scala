package code.service

import net.liftweb.common.{Empty, Box, Full}
import net.liftweb.util.{BaseField, FieldError}
import net.liftweb.mapper._
import code.helper.{Transformable, ForeignKeyField}

// TODO abstract "BaseMapper with IdPK"
trait CRUDifiable[CRUDType <: KeyedMapper[_, CRUDType]] {
  self: KeyedMetaMapper[_, CRUDType] =>

  def expose: Seq[(BaseField, Transformable[_])]

  // TODO finish method
  def FKSetup(f: ForeignKeyField[_, _], data: Any) {

  }

  def transformValues[T: Extractable](t: T): Seq[(String, Box[Any])] = {
    expose map {
      case (field, transform) => (field.name,
        transform(Extractor.extractField(t, field.name)))
    }
  }

  def setup(b: Box[CRUDType], kv: Seq[(String, Box[Any])]): Box[Mapper[_]] = {
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

  def createList[T: Extractable](t: T): List[Either[List[FieldError], BaseMapper with IdPK]] = {
    Extractor.extractModel(t, dbName.toLowerCase).map(create(_))
  }

  def read(id: String): Box[BaseMapper with IdPK] =
    for {
      item <- find(id)
    } yield item.asInstanceOf[BaseMapper with IdPK]

  def readField(id: String, fieldName: String): Box[(BaseMapper, BaseField)] =
    for {
      item <- find(id)
      field <- item.fieldByName[Any](fieldName)
    } yield (item, field)

  def readAll: Box[List[BaseMapper with IdPK]] = Full(findAll().map(_.asInstanceOf[BaseMapper with IdPK]))

  def readAllWithField[T](fieldName: String, value: T): Box[List[BaseMapper with IdPK]] =
    for {
      field <- fieldByName[T](fieldName)
      items <- Box !! findAll(By(field, value)).map(_.asInstanceOf[BaseMapper with IdPK])
    } yield items

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
