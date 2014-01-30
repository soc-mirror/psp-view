package psp
package core
package impl

/** Indexed
 */

abstract class IndexedImpl[+A](val size: Size) extends Indexed[A] {
  def isDefinedAt(index: Index): Boolean = 0 <= index && index < size.value
  @inline final def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < size.value) { f(elemAt(i)) ; i += 1 }
  }
  override def toString = Foreach stringify this
}

/** Indexable
 */
object StringIsIndexable extends IndexableImpl[String, Indexed, Char] {
  def convert(repr: String): Indexed[Char]     = Indexed pure repr
  def length(repr: String): Size               = Size(repr.length)
  def elemAt(repr: String)(index: Index): Char = repr charAt index.toInt
}
final class ArrayIsIndexable[A] extends IndexableImpl[Array[A], Indexed, A] {
  def convert(repr: Array[A]): Indexed[A]     = Indexed pure repr
  def length(repr: Array[A]): Size            = Size(repr.length)
  def elemAt(repr: Array[A])(index: Index): A = repr(index.toInt)
}
final class IndexedSeqIsIndexable[CC[X] <: IndexedSeq[X], A] extends IndexableImpl[CC[A], CC, A] {
  def convert(repr: CC[A]): CC[A]          = repr
  def length(repr: CC[A]): Size            = Size(repr.length)
  def elemAt(repr: CC[A])(index: Index): A = repr(index.toInt)
}
final class PspIndexedIsIndexable[A] extends IndexableImpl[Indexed[A], Indexed, A] {
  def convert(repr: Indexed[A]): Indexed[A]     = repr
  def length(repr: Indexed[A]): Size            = repr.size
  def elemAt(repr: Indexed[A])(index: Index): A = repr elemAt index
}

trait IndexableImpl[-Repr, CC0[X], A0] extends Indexable[Repr] {
  type CC[X]     = CC0[X]
  type A         = A0
  type Input[+X] = Indexed[X]
}

/** Foreachable
 */
final class ScalaTraversableIsForeachable[CC[X] <: Traversable[X], A] extends ForeachableImpl[CC[A], CC, A] {
  def convert(repr: CC[A]): CC[A]              = repr
  def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
}
final class PspForeachIsForeachable[A] extends ForeachableImpl[Foreach[A], Foreach, A] {
  def convert(repr: Foreach[A]): Foreach[A]         = repr
  def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
}
final class PspViewableIsForeachable[A] extends ForeachableImpl[ElementalView[A], Foreach, A] {
  def convert(repr: ElementalView[A]): Foreach[A]         = repr.toForeach
  def foreach(repr: ElementalView[A])(f: A => Unit): Unit = repr.toForeach foreach f
}

trait ForeachableImpl[-Repr, CC0[X], A0] extends Foreachable[Repr] {
  type CC[X]     = CC0[X]
  type A         = A0
  type Input[+X] = Foreach[X]
}
