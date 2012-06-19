package code.helper

import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.mapper.Like
import net.liftweb.util.Helpers.now
import java.util.Date
import code.model.User

trait Transformable[+T] {
  def apply(b: Box[String]): Box[T]
}

object Transformer {

  object Identity extends Transformable[String] {
    def apply(b: Box[String]): Box[String] = b
  }

  object ToInt extends Transformable[Int] {
    def apply(b: Box[String]): Box[Int] = {
      for (v <- b) yield v.toInt
    }
  }

  object ToLong extends Transformable[Long] {
    def apply(b: Box[String]): Box[Long] = {
      for (v <- b) yield v.toLong
    }
  }

  object ToDouble extends Transformable[Double] {
    def apply(b: Box[String]): Box[Double] = {
      for (v <- b) yield v.toDouble
    }
  }

  object ByEmail extends Transformable[Long] {
    def apply(v: Box[String]): Box[Long] = {
      User.find(Like(User.email, v openOr "")) match {
        case Full(f) => Full(f.id.is)
        case _ => Empty
      }
    }
  }

  object ToDate extends Transformable[Date] {

    import Formatter._

    override def apply(v: Box[String]): Box[Date] = {
      implicitly[Formattable[Date]].parse(v openOr "")
    }
  }

  object ToBoolean extends Transformable[Boolean] {
    override def apply(b: Box[String]): Box[Boolean] = {
      for (v <- b) yield v.toBoolean
    }
  }

  object Now extends Transformable[Date] {
    override def apply(b: Box[String]): Box[Date] = {
      Full(now)
    }
  }

}