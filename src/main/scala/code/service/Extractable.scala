package code.service

import net.liftweb.json._
import xml.Elem
import net.liftweb.common.{Full, Box}

trait Extractable[T] {
  def extractField(t: T, field: String): Box[String]

  def extractModel(t: T, modelName: String): List[T]
}

object Extractor {

  def extractField[T: Extractable](t: T, fieldName: String) = implicitly[Extractable[T]].extractField(t, fieldName)

  def extractModel[T: Extractable](t: T, modelName: String) = implicitly[Extractable[T]].extractModel(t, modelName)

  implicit object Json extends Extractable[JValue] {
    def extractField(t: JValue, field: String): Box[String] = {
      Box(for (JString(s) <- t \\ field) yield s)
    }

    def extractModel(t: JValue, modelName: String): List[JValue] = {
      (for (JArray(m) <- t \\ modelName) yield m).flatten
    }
  }

  implicit object Xml extends Extractable[Elem] {
    def extractField(t: Elem, field: String): Box[String] = {
      Full((t \\ field).text)
    }

    def extractModel(t: Elem, modelName: String): List[Elem] = {
      (t \\ modelName).seq.toList.map(_.asInstanceOf[Elem])
    }
  }

}
