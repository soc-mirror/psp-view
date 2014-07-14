package psp
package collection

import psp.core.{ Index, Size }

object LongRange {
  def until(start: Long, end: Long): LongRange = if (end < start) until(start, start) else new LongRange(start, end - 1, isInclusive = false)
  def to(start: Long, last: Long): LongRange   = if (last < start) until(start, start) else new LongRange(start, last, isInclusive = true)
}

final class LongRange private (val start: Long, val last: Long, isInclusive: Boolean) extends IndexedImpl[Long](Size(last - start + 1)) with DirectLeaf[Long] {
  def contains(x: Long): Boolean = start <= x && x <= last
  def isEmpty                    = last < start
  def end                        = last + 1
  def elemAt(i: Index): Long     = start + i
  override def toString          = if (isInclusive) s"$start to $last" else s"$start until $end"
}