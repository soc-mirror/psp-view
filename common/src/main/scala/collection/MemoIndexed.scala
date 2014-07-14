package psp
package collection

import java.{ lang => jl }
import psp.core.{ Size, SizeInfo, ArrayBuffer, LinkedBlockingQueue, nullAs }

final class MemoIndexed[+A](xs: Foreach[A]) extends Indexed[A] {
  @volatile private[this] var seen = 0
  @volatile private[this] var done = false

  private[this] val memo    = new ArrayBuffer[A]()
  private[this] val handoff = new LinkedBlockingQueue[A](1)
  private[this] lazy val thread  = spawn({ xs foreach handoff.put ; done = true })
  private[this] def spawn[A](body: => A): Unit = (new Thread() { override def run(): Unit = body }).start()
  private[this] def next(): A = handoff.poll match {
    case null => nullAs[A]
    case elem => memo += elem ; seen += 1 ; elem
  }
  private[this] def hasNext: Boolean = !done && (handoff.peek match {
    case null => Thread.`yield` ; hasNext
    case _    => true
  })

  def isDefinedAt(i: Long): Boolean = (i >= 0) && {
    thread
    while (i >= seen && hasNext) next()
    i < seen
  }

  def foreach(f: A => Unit): Unit = {
    def loop(i: Long): Unit = {
      if (isDefinedAt(i)) {
        f(elemAt(i))
        loop(i + 1)
      }
    }
    loop(0)
  }
  def apply(index: Long): A = elemAt(index)
  def elemAt(index: Long): A = if (isDefinedAt(index)) memo(index.toInt) else sys.error(s"Out of range: $index")
  def sizeInfo: SizeInfo = if (done) Size(seen).toInfo else Size(seen).toInfo.atLeast
  override def toString = "<memo>"
}

final class ZippedIndexed2[A, B, +C](left: Indexed[A], right: Indexed[B], f: (A, B) => C) extends Indexed[C] {
  def foreach(f: C => Unit): Unit = {
    var i = 0
    while (isDefinedAt(i)) { f(elemAt(i)); i += 1 }
  }
  def isDefinedAt(index: Long): Boolean = (left isDefinedAt index) && (right isDefinedAt index)
  def apply(index: Long): C             = elemAt(index)
  def elemAt(index: Long): C            = f(left elemAt index, right elemAt index)
  def sizeInfo: SizeInfo               = left.sizeInfo min right.sizeInfo
}

final class ZippedIndexed3[A, A1, A2, +B](xs1: Indexed[A], xs2: Indexed[A1], xs3: Indexed[A2], f: (A, A1, A2) => B) extends Indexed[B] {
  def foreach(f: B => Unit): Unit = {
    var i = 0
    while (isDefinedAt(i)) { f(elemAt(i)); i += 1 }
  }
  def isDefinedAt(index: Long): Boolean = (xs1 isDefinedAt index) && (xs2 isDefinedAt index) && (xs3 isDefinedAt index)
  def apply(index: Long): B             = elemAt(index)
  def elemAt(index: Long): B            = f(xs1(index), xs2(index), xs3(index))
  def sizeInfo: SizeInfo               = xs1.sizeInfo min xs2.sizeInfo min xs3.sizeInfo
}
