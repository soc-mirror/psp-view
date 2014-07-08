package psp
package collection

import scala.{ collection => sc }
import sc.{ mutable => scm }
import psp.core._

/** Compat
 */

final class ScalaIndexedSeqAsIndexed[+A](underlying: sc.IndexedSeq[A]) extends IndexedImpl[A](Size(underlying.size)) {
  def elemAt(index: Index) = underlying(index)
  override def toString = underlying.shortClass + " (wrapped)"
}

final class TraversableAsForeach[+A](underlying: Traversable[A]) extends Foreach[A] {
  def sizeInfo: SizeInfo          = SizeInfo(underlying)
  def foreach(f: A => Unit): Unit = underlying foreach f
  override def toString           = underlying.shortClass + " (wrapped)"
}

final class ForeachAsTraversable[+A](underlying: Foreach[A]) extends sc.immutable.Traversable[A] {
  def foreach[U](f: A => U): Unit = underlying foreach (x => f(x))
}

/** ArrowAssoc
 */

final class ArrowAssocInt(private val self: Int) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
}
final class ArrowAssocLong(private val self: Long) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
}
final class ArrowAssocDouble(private val self: Double) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
}
final class ArrowAssocChar(private val self: Char) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
}
final class ArrowAssocBoolean(private val self: Boolean) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
}
final class ArrowAssocRef[A](private val self: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
}

/** Direct
 */

abstract class IndexedImpl[+A](val size: Size) extends Direct[A] with HasPreciseSize {
  def sizeInfo = size
  @inline final def foreach(f: A => Unit): Unit = size foreachIndex (i => f(elemAt(i)))
  def isDefinedAt(index: Index): Boolean = size containsIndex index
  override def toString = Foreach stringify this
}

/** DirectAccess
 */
object StringIsCharSequence extends DirectAccessImpl[Char, String, Direct] {
  def length(repr: String): Size               = Size(repr.length)
  def elemAt(repr: String)(index: Index): Char = repr charAt index
}
object StringIsLineSequence extends DirectAccessImpl[Line, String, Direct] {
  def length(repr: String): Size               = wrap(repr).size
  def elemAt(repr: String)(index: Index): Line = wrap(repr) elemAt index
  def wrap(repr: String): DirectLeaf[Line]    = new PspStringAsLines(repr)
}

final class ArrayIsDirectAccess[A: ClassTag] extends DirectAccessImpl[A, Array[A], Direct] {
  def length(repr: Array[A]): Size            = Size(repr.length)
  def elemAt(repr: Array[A])(index: Index): A = repr(index)
}
final class IndexedSeqIsDirectAccess[CC[X] <: IndexedSeq[X], A] extends DirectAccessImpl[A, CC[A], CC] {
  def length(repr: CC[A]): Size            = Size(repr.length)
  def elemAt(repr: CC[A])(index: Index): A = repr(index)
}
final class PspIndexedIsDirectAccess[A0] extends DirectAccessImpl[A0, Direct[A0], Direct] {
  def length(repr: Direct[A0]): Size             = repr.size
  def elemAt(repr: Direct[A0])(index: Index): A0 = repr elemAt index
}

/** Foreachable
 */
final class TraversableIsForeachable[CC[X] <: Traversable[X], A] extends ForeachableImpl[A, CC[A], CC] {
  def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
}
final class PspForeachIsForeachable[A] extends ForeachableImpl[A, Foreach[A], Foreach] {
  def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
}
final class PspLinearIsSequentialAccess[A] extends SequentialAccessImpl[A, Linear[A], Linear] {
  def head(repr: Linear[A]): A          = repr.head
  def tail(repr: Linear[A]): Linear[A]  = repr.tail
  def isEmpty(repr: Linear[A]): Boolean = repr.isEmpty
}

trait DirectAccessImpl[A0, Repr, CC0[X]] extends DirectAccess[Repr] {
  @inline final def foreach(repr: Repr)(f: A => Unit): Unit = length(repr) foreachIndex (i => f(elemAt(repr)(i)))
  type CC[X] = CC0[X]
  type A     = A0
}

trait SequentialAccessImpl[A0, Repr, CC0[X]] extends SequentialAccess[Repr] {
  @inline def foreach(repr: Repr)(f: A => Unit): Unit = {
    @tailrec def loop(xs: Repr): Unit = if (!isEmpty(xs)) { f(head(xs)) ; loop(tail(xs)) }
    loop(repr)
  }
  type CC[X] = CC0[X]
  type A     = A0
}

trait ForeachableImpl[A0, Repr, CC0[X]] extends Foreachable[Repr] {
  type CC[X] = CC0[X]
  type A     = A0
}
