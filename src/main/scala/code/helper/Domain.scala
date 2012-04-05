package code.helper

import java.util.{ Date => javaDate }
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import net.liftweb.util.Helpers

object Date {
  private val datePattern = "dd/MM/yyyy HH:mm:ss"
  private val dateFormat = DateTimeFormat forPattern datePattern
  def format(date: DateTime): String = date toString dateFormat
  def parse(date: String): DateTime = dateFormat.parseDateTime(date)
  
  def now: DateTime = Helpers.now

  implicit def toDB(date: DateTime): javaDate = date.toDate
  implicit def toJoda(date: javaDate): DateTime = new DateTime(date)
  implicit def toString(date: DateTime): String = format(date)
}
