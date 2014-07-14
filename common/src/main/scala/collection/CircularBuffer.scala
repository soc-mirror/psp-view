package psp
package collection

import psp.core._

final class CircularBuffer[A](capacity: Size) extends Foreach[A] {
  assert(!capacity.isZero, capacity)

  private[this] val buffer  = new Array[Any](capacity.value.toInt)
  private[this] var pointer = 0
  private[this] var seen    = 0
  private[this] def current = bufferAt(pointer)

  private[this] def bufferAt(i: Long): A                  = buffer(i.toInt).castTo[A]
  private[this] def bufferUpdate(offset: Int, x: A): Unit = buffer(bufferIndex(offset).toInt) = x
  private[this] def bufferIndex(index: Long): Long        = (pointer + index) % capacity.value

  private[this] def indices: Direct[Long] = if (isFull) size.toIndexed map bufferIndex force else size.toIndexed
  private[this] def andThis(op: Unit): this.type = this
  private[this] def intSize = size.value

  def contents: Direct[A] = indices map bufferAt toIndexed
  def size: Size           = capacity min Size(seen)
  def isFull: Boolean      = size == capacity
  def sizeInfo: Precise    = size.toInfo

  def foreach(f: A => Unit): Unit = contents foreach f

  def push(x: A): A = {
    assert(isFull, this)
    try current finally this += x
  }

  def ++=(xs: Foreach[A]): this.type = andThis(xs foreach +=)

  def += (x: A): this.type = andThis {
    bufferUpdate(0, x)
    seen += 1
    pointer = bufferIndex(1).toInt
  }

  override def toString = s"CircularBuffer($size/$capacity)"
}

object CircularBuffer {
  def apply[A](capacity: Size): CircularBuffer[A] = new CircularBuffer[A](capacity)
}
