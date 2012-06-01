package code.helper

import net.liftweb.common.Box
import code.model._
import net.liftweb.mapper.Like
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.util.Helpers.now
import java.util.Date

sealed class Transform(p: Any) {
  def apply(b: Box[String]): Box[Any] = Full(p)
}

object Transform

object Identity extends Transform {
  override def apply(b: Box[String]): Box[String] = b
}

object ToInt extends Transform {
  override def apply(b: Box[String]): Box[Int] = {
    for (v <- b) yield v.toInt
  }
}

object ToLong extends Transform {
  override def apply(b: Box[String]): Box[Long] = {
    for (v <- b) yield v.toLong
  }
}

object ToDouble extends Transform {
  override def apply(b: Box[String]): Box[Double] = {
    for (v <- b) yield v.toDouble
  }
}

object ByUserName extends Transform {
  override def apply(v: Box[String]): Box[Long] = {
    User.find(Like(User.username, v openOr "")) match {
      case Full(f) => Full(f.id.is)
      case _ => Empty
    }
  }
}

object ByEmail extends Transform {
  override def apply(v: Box[String]): Box[Long] = {
    User.find(Like(User.email, v openOr "")) match {
      case Full(f) => Full(f.id.is)
      case _ => Empty
    }
  }
}

object ToDate extends Transform {
  import Formatter._
  override def apply(v: Box[String]): Box[Date] = {
    implicitly[Formattable[Date]].parse(v openOr "")
  }
}

object Now extends Transform {
  override def apply(b: Box[String]): Box[Date] = {
    Full(now)
  }
}

object StaticString {
  def apply(s: String): Transform = {
    new Transform(s)
  }
}
