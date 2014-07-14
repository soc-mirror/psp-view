package psp
package tests

import core._
import collection._
import compat.ScalaNative
import utest._

object CountOperationsSpec extends TestSuite {
  val tests = TestSuite {
    "composite" - {
      "agreement" - assert(results forall (_.isAgreement))
    }
  }
  showResults()

  type LongView = api.View[Long]

  lazy val tupleFlatMap: Long => Foreach[Long] = ((x: Long) => Foreach.elems(x, x)) labeled "(x, x)"
  lazy val isEven: Long => Boolean             = ((x: Long) => x % 2 == 0) labeled "isEven"
  lazy val timesThree: Long => Long            = ((x: Long) => x * 3) labeled "*3"
  lazy val collectDivSix: Long =?> Long        = labelpf("%/6")({ case x: Long if x % 6 == 0 => x / 6 })

  def max    = 1000
  def numOps = 3
  def basicOps = List[LongView => LongView](
    _ drop 5,
    _ dropRight 11,
    _ slice Interval(7, 41),
    _ take 13,
    _ takeRight 17,
    _ flatMap tupleFlatMap,
    _ filter isEven,
    _ map timesThree,
    _ collect collectDivSix
  )

  def scalaLongRange = scala.collection.immutable.NumericRange.inclusive[Long](1, max, 1)

  def usCollections = List[LongView](
    PspList.to(1, max).m,
    PspList.to(1, max).m sized Size(max),
    LongRange.to(1, max).m,
    LongRange.to(1, max / 2).m ++ LongRange.to(max / 2 + 1, max)
  )
  def themCollections = List[LongView](
    ScalaNative(scalaLongRange.toList.view),
    ScalaNative(scalaLongRange.toStream),
    ScalaNative(scalaLongRange.toStream.view),
    ScalaNative(scalaLongRange.view),
    ScalaNative(scalaLongRange.toVector.view)
  )
  def rootCollections = usCollections ++ themCollections

  def compositesOfN(n: Int): List[LongView => LongView] = (
    (basicOps combinations n flatMap (_.permutations.toList)).toList.distinct
      map (xss => xss reduceLeft (_ andThen _))
  )

  class CollectionResult(viewFn: LongView => LongView, xs: LongView) {
    val view    = viewFn(xs)
    val result  = view take 3 mkString ", "
    val count   = xs.calls
    def display = view.viewChain reverseMap (v => fmtdesc(v.description)) filterNot (_.trim.length == 0) mkString ("<xs>  ", " ", "")

    def fmtdesc(description: String): String = description indexOf ' ' match {
      case -1  => "%-15s" format description
      case idx => "%-7s %-7s".format(description.substring(0, idx), description.substring(idx + 1))
    }
    override def toString = display
  }

  class CompositeOp(viewFn: LongView => LongView) {
    val us: List[CollectionResult]   = usCollections map (xs => new CollectionResult(viewFn, xs))
    val control: CollectionResult    = new CollectionResult(viewFn, ScalaNative(scalaLongRange.toList))
    val them: List[CollectionResult] = themCollections map (xs => new CollectionResult(viewFn, xs))
    val all: List[CollectionResult]  = us ++ (control +: them)

    def usCounts   = us map (_.count)
    def themCounts = them map (_.count)
    def allResults = all map (_.result)
    def allCounts  = all map (_.count)

    def usAverage   = usCounts.sum / us.size.toDouble
    def themAverage = themCounts.sum / them.size.toDouble
    def ratioDouble = themAverage / usAverage
    def ratio       = if (ratioDouble == Double.PositiveInfinity) "Inf" else "%.2f" format ratioDouble

    def headResult  = us.head

    def fmtdesc(description: String): String = description indexOf ' ' match {
      case -1  => "%-15s" format description
      case idx => "%-7s %-7s".format(description.substring(0, idx), description.substring(idx + 1))
    }
    def headView    = us.head.toString
    def isAgreement = allResults.distinct.size == 1
    def display     = (
         !isAgreement
      || (usCounts.distinct.size == usCollections.size)
      || (allCounts.distinct.size > 4)
      || (allCounts.distinct.size > 2 && ratioDouble < 1.3d)
      || true
    )
    def countsString   = allCounts map ("%7s" format _) mkString " "
    def resultsString  = if (isAgreement) headResult.result else "!!! " + failedString
    def failedString   = {
      val grouped = all.zipWithIndex groupBy { case (x, i) => x.result }
      val lines = grouped.toList map { case (res, pairs) => "%20s:  %s".format(pairs.map(_._2).mkString(" "), res) }
      lines.mkString("\n  ", "\n  ", "\n")
    }
    def padding        = " " * (headView.length + 2)
    def sortKey        = ((-ratioDouble, usCounts.min))

    override def toString = "%s  %6s %s  //  %s".format(headView, ratio, countsString, resultsString)
  }

  lazy val results = compositesOfN(numOps) map (fn => new CompositeOp(fn))

  private def showResults() {
    val (show, noshow)    = results partition (_.display)
    val banner: String    = List("Improve", "Linear", "Sized", "Direct", "50/50", "<EAGER>", "ListV", "Stream", "StreamV", "RangeV", "VectorV") map ("%7s" format _) mkString " "
    val underline: String = banner.toCharArray map (ch => if (ch == ' ') ' ' else '-') mkString ""
    val padding           = show.head.padding

    println(pp"""
      |Basis sequence was 1 to $max
      |Displaying ${show.size}/${results.size} results - omitted ${noshow.size} less interesting results
      |
      |$padding$banner
      |$padding$underline
      |${show mkString EOL}
      |""".stripMargin)
  }

}
