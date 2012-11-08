package code.service

import net.liftweb.mapper.{KeyedMetaMapper, KeyedMapper}

trait DomainService[ServiceType <: KeyedMapper[_, ServiceType]] extends Service
with CRUDifiable[ServiceType]
with Plotifiable[Long, ServiceType] {
  self: KeyedMetaMapper[_, ServiceType] =>

  def path = dbTableName :: Nil

  import Extractor._
  import Converter._
  import Viewer._

  serve {
    servicePath prefix {
      // create
      case Nil XmlPut xml -> _ =>
        toXmlResp(toView(create(xml)))
      case Nil JsonPut json -> _ =>
        toJsonResp(toView(create(json)))

      // create list
      case "list" :: Nil XmlPut xml -> _ =>
        toXmlResp(toListView(createList(xml)))
      case "list" :: Nil JsonPut json -> _ =>
        toJsonResp(toListView(createList(json)))
    }
  }

  serve {
    servicePath prefix {
      // update
      case id :: Nil XmlPost xml -> _ =>
        toXmlResp(toView(update(id, xml)))
      case id :: Nil JsonPost json -> _ =>
        toJsonResp(toView(update(id, json)))
    }
  }

  serve {
    servicePath prefix {
      // read
      case id :: Nil XmlGet _ =>
        for (item <- read(id)) yield toXmlResp(toView(item))
      case id :: Nil JsonGet _ =>
        for (item <- read(id)) yield toJsonResp(toView(item))

      // read field
      case id :: field :: Nil XmlGet _ =>
        for (f <- readField(id, field)) yield toXmlResp(toView(f))
      case id :: field :: Nil JsonGet _ =>
        for (f <- readField(id, field)) yield toJsonResp(toView(f))

      // read all with field
      // TODO modify the method to accept only strings
      case "list" :: field :: value :: Nil XmlGet _ =>
        for (items <- readAllWithField[Long](field, value.toLong)) yield toXmlResp(toListView(items))
      case "list" :: field :: value :: Nil JsonGet _ =>
        for (items <- readAllWithField[Long](field, value.toLong)) yield toJsonResp(toListView(items))

      // read all
      case Nil XmlGet _ =>
        for (items <- readAll) yield toXmlResp(toListView(items))
      case Nil JsonGet _ =>
        for (items <- readAll) yield toJsonResp(toListView(items))
    }
  }

  serve {
    servicePath prefix {
      // delete
      case id :: Nil XmlDelete _ =>
        for (item <- delete(id)) yield toXmlResp(toView(item))
      case id :: Nil JsonDelete _ =>
        for (item <- delete(id)) yield toJsonResp(toView(item))
    }
  }
}
