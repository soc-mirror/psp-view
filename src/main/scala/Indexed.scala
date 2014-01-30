package psp
package core

import impl._

trait OpenIndexed[+A] extends Any with Foreach[A] {
  def isDefinedAt(index: Index): Boolean
  def elemAt(index: Index): A
  // TODO - move onto Ops
  def zip[B](that: OpenIndexed[B]): ZippedIndexed[A, B] = new ZippedIndexed(this, that)
}
trait Indexed[+A] extends Any with OpenIndexed[A] with HasPreciseSize

trait HasContains[-A] extends Any {
  def contains(x: A): Boolean
}
trait Invariant[A] extends Any

final class PureIndexed[+A](size: Size, indexFn: Long => A) extends IndexedImpl[A](size) {
  def elemAt(index: Index): A = indexFn(index)
}

object Indexed {
  implicit def newBuilder[A] : PspCanBuild[A, Indexed[A]] = new PspCanBuildImpl(_.toIndexed)

  object Empty extends IndexedImpl[Nothing](Zero) with HasStaticSize {
    def elemAt(index: Index): Nothing = failEmpty(pp"$this($index)")
    override def toString = "<empty>"
  }

  /** Immutability (particularly of Arrays) is on the honor system. */
  def pure[Repr](xs: Repr)(implicit tc: Indexable[Repr]): Indexed[tc.A] = pure(tc length xs, index => (tc elemAt xs)(index))
  def pure[A](size: Size, indexFn: Index => A): Indexed[A]              = new PureIndexed(size, indexFn)

  def fill[A](times: Int)(body: => A): Indexed[A] = {
    val buf = Vector.newBuilder[A]
    Interval(0, times) foreach (_ => buf += body)
    pure(buf.result)
  }
  def empty[A] : Indexed[A] = Foreach.Empty
  def elems[A](xs: A*): Indexed[A] = xs match {
    case xs: WrappedArray[A] => pure(xs.array)
    case _                   => pure(xs.toVector)
  }
}
object LongRange {
  def until(start: Long, end: Long): LongRange = if (end < start) until(start, start) else new LongRange(start, end - 1, isInclusive = false)
  def to(start: Long, last: Long): LongRange   = if (last < start) until(start, start) else new LongRange(start, last, isInclusive = true)
}

final class LongRange private (val start: Long, val last: Long, isInclusive: Boolean) extends IndexedImpl[Long](Size(last - start + 1)) with HasContains[Long] {
  def contains(x: Long): Boolean = start <= x && x <= last
  def isEmpty               = last < start
  def end                   = last + 1
  def elemAt(i: Index): Long = start + i
  override def toString     = if (isInclusive) s"$start to $last" else s"$start until $end"
}
