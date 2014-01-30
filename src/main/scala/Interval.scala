package psp
package core

import Interval.Empty

final class Interval private (val start: Index, val end: Index) {
  require(start >= 0 && end >= 0 && start <= end, pp"$start, $end")

  def -(n: Long): Interval = Interval(start - n, end - n)
  def +(n: Long): Interval = Interval(start + n, end + n)

  // def reverse: Interval           = new Interval(start, end, !isReversed)
  def drop(n: Long): Interval      = if (n <= 0) this else Interval(start + n, end)
  def dropRight(n: Long): Interval = if (n <= 0) this else Interval(start, end - n)
  def take(n: Long): Interval      = if (n <= 0) Empty else if (longSize <= n) this else Interval(start, start + n)
  def takeRight(n: Long): Interval = if (n <= 0) Empty else if (longSize <= n) this else Interval(end - n, end)
  def contains(index: Index)      = start <= index && index < end

  def intersect(other: Interval): Interval = Interval(start max other.start, end min other.end)

  def slice(s: Long, e: Long): Interval  = if (s < 0) slice(0, e) else if (e <= s) Empty else this drop s take (e - s)
  def slice(range: Interval): Interval = slice(range.start, range.end)

  @inline final def foreach(f: Index => Unit): Unit = {
    var i = start
    while (i < end) { f(i) ; i += 1 }
  }

  def isEmpty                 = longSize == 0
  def size: Size              = Size(longSize)
  private def longSize: Long    = end - start
  def firstIndex: Index       = if (isEmpty) NoIndex else start
  def lastIndex: Index        = if (isEmpty) NoIndex else end - 1
  def toScalaRange: Range     = scala.collection.immutable.Range(start.toInt, end.toInt, 1)
  def toIndexed: Indexed[Long] = LongRange.until(start, end)
  override def toString       = if (isEmpty) "[0,0)" else pp"[$start,$end)" //pp"[$firstIndex,$lastIndex]"
}

object Interval {
  val Empty = new Interval(0, 0)
  val Full  = new Interval(0, Int.MaxValue)

  def apply(start: Index, end: Index): Interval =
    if (end <= start || end <= 0) Empty else new Interval(start max 0, end)

  def unapply(x: Interval) = Some((x.start, x.end))
}
