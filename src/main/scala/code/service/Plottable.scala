package code.service

import scala.math.sin
import java.util.Date
import net.liftweb.sitemap.{Menu, Loc}
import scala.xml.NodeSeq
import net.liftweb.proto.Crudify
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{JFreeChart, ChartFactory}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.data.time.{TimeSeriesCollection, TimeSeries, Millisecond}
import net.liftweb.http.js.JE.{Num, JsObj}
import net.liftweb.http.js.JsCmd
import net.liftweb.widgets.flot.{
Flot,
FlotSerie,
FlotPointsOptions,
FlotOptions,
FlotLinesOptions,
FlotGridOptions,
FlotAxisOptions
}
import net.liftweb.common.{Empty, Full, Box}
import net.liftweb.util.Helpers.tryo
import net.liftweb.mapper._
import code.helper.Formattable

case class Series(label: Box[String], data: Set[Box[(Double, Double)]])

trait Chartable[T] {
  def toSeries(t: T): List[Series]

  def toChart(t: T): JFreeChart

  implicit def toJsSerie(entry: Series): FlotSerie = {
    new FlotSerie {
      override val data = entry.data.filter(!_.isEmpty).map(_.open_!).toList
      override val label = entry.label
    }
  }

  implicit def toJsSerieList(entries: List[Series]): List[FlotSerie] = {
    entries.map(toJsSerie)
  }

  def jsOptions(s: List[Series]): FlotOptions

  def toJs(t: T, placeholder: String) = {
    val s = toSeries(t)
    Flot.renderJs(placeholder, s, jsOptions(s), JsCmd.unitToJsCmd())
  }

}

object ChartBuilder {

  def toSeries[T: Chartable](t: T) = implicitly[Chartable[T]].toSeries(t)

  def toChart[T: Chartable](t: T) = implicitly[Chartable[T]].toChart(t)

  implicit def toJs[T: Chartable](t: T, placeholder: String) = implicitly[Chartable[T]].toJs(t, placeholder)

  implicit object Blank extends Chartable[BlankPlot] {

    def toSeries(t: BlankPlot) = new Series(Empty, Set.empty) :: Nil

    def toChart(t: BlankPlot) = {
      ChartFactory.createBarChart(
        toSeries(t).head.label getOrElse "",
        t.ind,
        t.dep,
        new DefaultCategoryDataset,
        PlotOrientation.VERTICAL,
        false,
        true,
        false)
    }

    def jsOptions(s: List[Series]) = new FlotOptions {}
  }

  implicit object Sine extends Chartable[SinePlot] {
    // TODO remove hardcoding
    def toSeries(t: SinePlot) = new Series(Full("SinePlot Wave"), (for (i <- List.range(0, 140, 5))
    yield Full(i / 10.0, sin(i / 10.0))).toSet) :: Nil

    def toChart(t: SinePlot) = {
      val s = toSeries(t)
      val dataset = new XYSeries(s.head.label openOr "")
      s.head.data map {
        case Full((x, y)) => dataset.addOrUpdate(x, y)
        case _ => Unit
      }
      ChartFactory.createTimeSeriesChart(
        t.dep + " over " + t.ind,
        t.ind,
        t.dep,
        new XYSeriesCollection(dataset),
        true,
        true,
        false)
    }

    def jsOptions(s: List[Series]) = new FlotOptions {}
  }

  implicit object Time extends Chartable[TimePlot] {
    def toSeries(t: TimePlot) = {
      import code.helper.Formatter._
      new Series(Full(t.dep), t.source.map {
        v =>
          val dataMap = v.items.toMap
          val date = Joda.parse(dataMap.getOrElse(t.ind, "")).map(_.getTime.toDouble)
          val value = tryo { dataMap(t.dep).toDouble }
          (date, value) match {
            case (Full(x), Full(y)) => Full((x, y))
            case _ => Empty
          }
      }.toSet) :: Nil
    }

    def toChart(t: TimePlot) = {
      val s = toSeries(t)
      val dataset = new TimeSeries(s.head.label openOr "")
      s.head.data map {
        case Full((x, y)) =>
          dataset.addOrUpdate(new Millisecond(new Date(x.toLong)), y)
        case _ => Unit
      }
      ChartFactory.createTimeSeriesChart(
        t.dep + " over " + t.ind,
        t.ind,
        t.dep,
        new TimeSeriesCollection(dataset),
        true,
        true,
        false)
    }

    def jsOptions(s: List[Series]) = new FlotOptions {
      override val grid = Full(new FlotGridOptions {
        override val clickable = Full(true)
        override val hoverable = Full(true)
      })
      override val xaxis = Full(new FlotAxisOptions {
        override val mode = Full("time")
      })
      override val points = Full(new FlotPointsOptions {
        override val show = Full(true)
      })
      override val lines = Full(new FlotLinesOptions {
        override val show = Full(true)
      })
    }

  }

  implicit object Group extends Chartable[GroupPlot] {
    // TODO make gets typesafe on Map
    def toSeries(t: GroupPlot) = {
      val dataByKey = t.source.map(_.items.toMap).groupBy(_.getOrElse(t.ind, ""))
      (dataByKey map {
        case (k, v) => {
          val index = dataByKey.keySet.toList.indexOf(k) + 1.0
          new Series(Full(k), Set(Full((index, v.size.toDouble))))
        }
      }).toList
    }

    def toChart(t: GroupPlot) = {
      val s = toSeries(t)
      val dataset = new DefaultCategoryDataset()
      s map {
        item =>
          item.data.head match {
            case Full(p) => dataset.setValue(p._2, item.label openOr "", item.label openOr "")
            case _ => Unit
          }
      }
      ChartFactory.createBarChart(
        t.ind + " " + t.dep,
        t.ind,
        t.dep,
        dataset,
        PlotOrientation.VERTICAL,
        false,
        true,
        false)
    }

    def jsOptions(s: List[Series]) = new FlotOptions {
      override val series = Full(Map(
        "stack" -> Num(0),
        "lines" -> JsObj(
          "show" -> false,
          "steps" -> false),
        "bars" -> JsObj(
          "show" -> true,
          "barWidth" -> 0.9,
          "align" -> "center")))
      override val xaxis = Full(new FlotAxisOptions {
        override val ticks = (0 to s.count(d => true) - 1).map(_.toDouble).toList
      })
      override val modeSelection = Full("x")
    }
  }

}

sealed trait Chart

case class BlankPlot(ind: String,
                 dep: String,
                 indRange: (String, String)) extends Chart

case class SinePlot(source: List[View],
                    ind: String,
                    dep: String,
                    indRange: (String, String)) extends Chart

case class TimePlot(source: List[View],
                    ind: String,
                    dep: String,
                    indRange: (String, String)) extends Chart

case class GroupPlot(source: List[View],
                     ind: String,
                     dep: String,
                     indRange: (String, String)) extends Chart

trait Plottable[_, PlotType <: KeyedMapper[_, PlotType]] extends Crudify {
  self: PlotType with KeyedMetaMapper[_, PlotType] =>

  import Viewer._
  import ChartBuilder._
  import code.helper.Formatter._

  def getField(fieldName: String) =
  // By_>=(fieldByName[Any]("dependent").open_!, 3.0)
    fieldOrder.map(f => (f.name, f)).toMap.getOrElse(fieldName, Unit).asInstanceOf[MappedField[Any, PlotType]]

  def getWithRange(fieldName: String, range: (String, String)) =
    findAll(By_>=(getField(fieldName), implicitly[Formattable[Date]].parse(range._1).open_!),
      By_<=(getField(fieldName), implicitly[Formattable[Date]].parse(range._2).open_!))

  def plotToChart(plotKind: String, ind: String, dep: String, range: (String, String)): JFreeChart = {
    plotKind match {
      case "group" => toChart(GroupPlot(toListView(findAll().map(_.asInstanceOf[BaseMapper])), ind, dep, range))
      case "time" =>
        toChart(TimePlot(toListView(getWithRange(ind, range).map(_.asInstanceOf[BaseMapper])), ind, dep, range))
      case "sine" => toChart(SinePlot(View("SinePlot Wave", (for {i <- List.range(0, 140, 5)}
      yield (i / 10.0, sin(i / 10.0))).map {
        case (k, v) => (k.toString, v.toString)
      }) :: Nil, ind, dep, range))
      case _ => toChart(BlankPlot("X", "Y", range))
    }
  }

  def plotToJs(plotKind: String, ind: String, dep: String, range: (String, String)): JsCmd = {
    plotKind match {
      case "group" => toJs(GroupPlot(toListView(findAll().map(_.asInstanceOf[BaseMapper])), ind, dep, range), placeholder)
      case "time" =>
        toJs(TimePlot(toListView(getWithRange(ind, range).map(_.asInstanceOf[BaseMapper])), ind, dep, range), placeholder)
      case "sine" => toJs(SinePlot(View("SinePlot Wave", (for (i <- List.range(0, 140, 5))
      yield (i / 10.0, sin(i / 10.0))).map {
        case (k, v) => (k.toString, v.toString)
      }) :: Nil, ind, dep, range), placeholder)
      case _ => toJs(BlankPlot("X", "Y", range), placeholder)
    }
  }

  abstract override def menus = super.menus ::: plotMenuLoc.toList

  def plotMenuLoc: Box[Menu] = {
    Full(Menu(Loc("Plot " + _dbTableNameLC, List(_dbTableNameLC) ::: "plot" :: Nil,
      "Plot " + _dbTableNameLC, Loc.Template(() => plotTemplate()))))
  }

  val placeholder = "placeholder"

  def plotTemplate(): NodeSeq = pageWrapper(_plotTemplate)

  def _plotTemplate = {
    <script data-lift="head" id="flot" src="/classpath/flot/jquery.flot.js" type="text/javascript"></script>
        <link data-lift="head" type="text/css" rel="stylesheet" href="/classpath/flot/jquery.flot.css"/>
      <h2>Plot
        {_dbTableNameLC}
      </h2>
      <lift:Plotter plot={_dbTableNameLC}>
        <label for="plotKind">Plot kind:</label> <input type="text" id="plotKind"/>
        <div>
          <b>Axis:</b>
          <label for="ind">independent:</label> <input type="text" id="ind"/>
          <label for="dep">dependent:</label> <input type="text" id="dep"/>
        </div>
          <br/>
        <div>
          <b>Range:</b>
          <label for="start">start:</label> <input type="text" id="start"/>
          <label for="end">end:</label> <input type="text" id="end"/>
        </div>
        <div id={placeholder} style="width: 600px; height: 400px;"></div>
        <span id="results"></span>
      </lift:Plotter>
  }
}
