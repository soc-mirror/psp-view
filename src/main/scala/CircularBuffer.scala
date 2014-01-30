package psp
package core

final class CircularBuffer[A](capacity: Size) extends Foreach[A] {
  assert(capacity > Zero, capacity)

  private[this] val buffer  = new Array[Any](capacity.toInt)
  private[this] var pointer = 0L
  private[this] var seen    = 0L
  private[this] def current = bufferAt(pointer)

  private[this] def bufferAt(i: Long): A                   = buffer(i.toInt).castTo[A]
  private[this] def bufferUpdate(offset: Long, x: A): Unit = buffer(bufferIndex(offset).toInt) = x
  private[this] def bufferIndex(index: Long): Long          = (pointer + index) % capacity.value

  private[this] def indices: Indexed[Long] = if (isFull) size.toIndexed map bufferIndex else size.toIndexed
  private[this] def andThis(op: Unit): this.type = this
  private[this] def intSize = size.value.toInt

  def contents: Indexed[A] = indices map bufferAt toIndexed
  def size     = capacity min Size(seen)
  def isFull   = size == capacity
  def sizeInfo = SizeInfo.Precise(size)

  def foreach(f: A => Unit): Unit = contents foreach f

  def push(x: A): A = {
    assert(isFull, this)
    try current finally this += x
  }

  def ++=(xs: Foreach[A]): this.type = andThis(xs foreach +=)

  def += (x: A): this.type = andThis {
    bufferUpdate(0, x)
    seen += 1
    pointer = bufferIndex(1)
  }

  override def toString = s"CircularBuffer($size/$capacity)"
}

object CircularBuffer {
  def apply[A](capacity: Size): CircularBuffer[A] = new CircularBuffer[A](capacity)
}
