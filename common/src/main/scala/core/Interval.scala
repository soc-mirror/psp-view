package psp
package core

import psp.collection.{ Direct, LongRange }
import Interval.Empty

final class Interval private (val start: Index, val end: Index) {
  require(start >= 0 && end >= 0 && start <= end, pp"$start, $end")

  def -(n: Long): Interval = Interval(start - n, end - n)
  def +(n: Long): Interval = Interval(start + n, end + n)

  def reverse: Direct[Long]        = Direct.pure(size, idx => size.value - 1 - idx)
  def drop(n: Long): Interval      = if (n <= 0) this else Interval(start + n, end)
  def dropRight(n: Long): Interval = if (n <= 0) this else Interval(start, end - n)
  def take(n: Long): Interval      = if (n <= 0) Empty else if (intSize <= n) this else Interval(start, start + n)
  def takeRight(n: Long): Interval = if (n <= 0) Empty else if (intSize <= n) this else Interval(end - n, end)
  def contains(index: Index)       = start <= index && index < end

  def intersect(other: Interval): Interval = Interval(math.max(start, other.start), math.min(end, other.end))

  def slice(s: Long, e: Long): Interval  = if (s < 0) slice(0, e) else if (e <= s) Empty else this drop s take (e - s)
  def slice(range: Interval): Interval   = slice(range.start, range.end)

  @inline final def foreach(f: Index => Unit): Unit = {
    var i = start
    while (i < end) { f(i) ; i += 1 }
  }

  def isEmpty                 = intSize == 0
  def size: Size              = Size(intSize)
  private def intSize: Long   = end - start
  def firstIndex: Index       = if (isEmpty) NoIndex else start
  def lastIndex: Index        = if (isEmpty) NoIndex else end - 1
  def toScalaRange: Range     = scala.collection.immutable.Range(start.toInt, end.toInt, 1)
  def toIndexed: Direct[Long] = LongRange.until(start, end)
  override def toString       = if (isEmpty) "[0,0)" else pp"[$start,$end)" //pp"[$firstIndex,$lastIndex]"
}

object Interval extends ((Index, Index) => Interval) {
  val Empty = new Interval(0, 0)
  val Full  = new Interval(0, Int.MaxValue)

  def apply(start: Index, end: Index): Interval =
    if (end <= start || end <= 0) Empty else new Interval(math.max(start, 0), end)

  def unapply(x: Interval) = Some((x.start, x.end))
}
