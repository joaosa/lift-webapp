package code.helper

import java.util.Date
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import net.liftweb.util.Helpers

trait Formattable[T] {
  def format(t: T): String

  def parse(s: String): T
}

object Formattable {

  def now: DateTime = Helpers.now

  def format[T: Formattable](t: T): String = implicitly[Formattable[T]].format(t)

  def parse[T: Formattable](s: String): T = implicitly[Formattable[T]].parse(s)

  implicit def toDate(date: DateTime): Date = date.toDate

  implicit def toJoda(date: Date): DateTime = new DateTime(date)

  private val datePattern = "yyyy-MM-dd HH:mm:ss Z"
  private val dateFormat = DateTimeFormat forPattern datePattern

  implicit object Joda extends Formattable[DateTime] {
    def format(t: DateTime) = t toString dateFormat

    def parse(s: String) = dateFormat.parseDateTime(s)
  }

  implicit object Date extends Formattable[Date] {
    def format(t: Date) = Joda.format(t)

    def parse(s: String) = Joda.parse(s)
  }

}
