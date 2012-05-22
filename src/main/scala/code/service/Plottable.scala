package code.service

import net.liftweb.common.{Full, Box}
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
import net.liftweb.mapper.{BaseMapper, KeyedMapper, KeyedMetaMapper}

case class Series(label: String, data: Set[(Double, Double)])

sealed trait Chart {

  def toSeries: List[Series]

  def toExport: JFreeChart

  def jsOptions: FlotOptions = new FlotOptions {}

  implicit def toJsSerie(entry: Series): FlotSerie = {
    new FlotSerie {
      override val data = entry.data.toList
      override val label = Full(entry.label)
    }
  }

  implicit def toJsSerieList(entries: List[Series]): List[FlotSerie] = {
    entries.map(toJsSerie)
  }

  def toJs(placeholder: String) =
    Flot.renderJs(placeholder, toSeries, jsOptions, JsCmd.unitToJsCmd())
}

case object Blank extends Chart {

  def ind: String = "X"

  def dep: String = "Y"

  def toSeries = new Series("", Set.empty) :: Nil

  def toExport =
    ChartFactory.createBarChart(
      "Empty",
      ind,
      dep,
      new DefaultCategoryDataset,
      PlotOrientation.VERTICAL,
      false,
      true,
      false)
}

case class SinePlot(data: List[View],
                    ind: String,
                    dep: String,
                    indRange: Box[(String, String)])
  extends Chart {
  def toSeries = new Series("SinePlot Wave", (for (i <- List.range(0, 140, 5))
  yield (i / 10.0, sin(i / 10.0))).toSet) :: Nil

  def toExport = {
    val dataset = new XYSeries(toSeries.head.label)
    toSeries.head.data map {
      case (x, y) => dataset.addOrUpdate(x, y)
    }
    ChartFactory.createTimeSeriesChart(
      dep + " over " + ind,
      ind,
      dep,
      new XYSeriesCollection(dataset),
      true,
      true,
      false)
  }
}

case class TimePlot(source: List[View],
                    ind: String,
                    dep: String,
                    indRange: Box[(String, String)])
  extends Chart {
  def toSeries = {
    import code.helper.Formatter._
    new Series(dep, source.map {
      s =>
        val dataMap = s.items.toMap
        val date = Joda.parse(dataMap(ind)).getTime.toDouble
        val value = dataMap(dep).toDouble
        (date, value)
    }.toSet) :: Nil
  }

  def toExport = {
    lazy val series = toSeries
    val dataset = new TimeSeries(series.head.label)
    series.head.data map {
      case (x, y) =>
        dataset.addOrUpdate(new Millisecond(new Date(x.toLong)), y)
    }
    ChartFactory.createTimeSeriesChart(
      dep + " over " + ind,
      ind,
      dep,
      new TimeSeriesCollection(dataset),
      true,
      true,
      false)
  }

  override def jsOptions = new FlotOptions {
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

case class GroupPlot(source: List[View],
                     ind: String,
                     dep: String,
                     indRange: Box[(String, String)])
  extends Chart {
  def toSeries = {
    val dataByKey =
      source.map(_.items.toMap).groupBy(_(ind))
    (dataByKey map {
      case (k, v) => {
        val index = dataByKey.keySet.toList.indexOf(k) + 1
        new Series(k, Set((index.toDouble, v.size.toDouble)))
      }
    }).toList
  }

  def toExport = {
    val dataset = new DefaultCategoryDataset()
    toSeries map {
      item =>
        dataset.setValue(item.data.head._2, item.label, item.label)
    }
    ChartFactory.createBarChart(
      ind + " " + dep,
      ind,
      dep,
      dataset,
      PlotOrientation.VERTICAL,
      false,
      true,
      false)
  }

  override def jsOptions = new FlotOptions {
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
      override val ticks = (0 to toSeries.count(d => true) - 1).map(_.toDouble).toList
    })
    override val modeSelection = Full("x")
  }
}

trait Plottable[_, PlotType <: KeyedMapper[_, PlotType]] extends Crudify {
  self: PlotType with KeyedMetaMapper[_, PlotType] =>

  def plot(plotKind: String, ind: String, dep: String, range: Box[(String, String)]): Chart = {
    import Viewer._
    plotKind match {
      case "group" => GroupPlot(toListView(findAll().map(_.asInstanceOf[BaseMapper])), ind, dep, range)
      case "time" => TimePlot(toListView(findAll().map(_.asInstanceOf[BaseMapper])), ind, dep, range)
      case "sine" => SinePlot(View("SinePlot Wave", (for (i <- List.range(0, 140, 5))
      yield (i / 10.0, sin(i / 10.0))).map {
        case (k, v) => (k.toString, v.toString)
      }) :: Nil, ind, dep, range)
      case _ => Blank
    }
  }

  override def menus = List(showAllMenuLoc, createMenuLoc, viewMenuLoc,
    editMenuLoc, deleteMenuLoc, plotMenuLoc).flatMap(x => x)

  def plotMenuLoc: Box[Menu] = {
    Full(Menu(Loc("Plot " + _dbTableNameLC, List(_dbTableNameLC) ::: "plot" :: Nil,
      "Plot " + _dbTableNameLC, Loc.Template(() => plotTemplate()))))
  }

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
        <button id="trigger">Plot</button>
        <div id="placeholder" style="width: 600px; height: 400px;"></div>
        <span id="results"></span>
      </lift:Plotter>
  }
}
