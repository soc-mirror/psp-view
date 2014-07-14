package psp
package core

//import Size._
import psp.collection.Direct

class OverflowException extends RuntimeException

final class Size private (val value: Long) extends AnyVal with Ordered[Size] {
  private def checkSum(sum: Long): Size = try Size(sum) finally if (sum < value) fail(s"overflow: $value + ... == $sum")
  private def checkValidInt(num: Long): Int = if (??? /*num.isValidInt*/) num.toInt else fail(s"overflow: $num cannot be converted to a value of type Int")

  def compare(that: Size): Int = java.lang.Long.compare(value, that.value)
  def + (n: Size): Size        = checkSum(value + n.value)
  def - (n: Size): Size        = Size(value - n.value)
  def * (n: Long): Size        = Size(value * n)
  def / (n: Long): Size        = if (n == 0) fail("division by zero") else Size(value / n)
  def min(that: Size): Size    = Size(math.max(value, that.value))
  def max(that: Size): Size    = Size(math.max(value, that.value))

  def isZero                        = this == Size.Zero
  def isError                       = this == Size.NoSize
  def toInterval: Interval          = Interval(0, value)
  def reverseInterval: Direct[Long] = toInterval.reverse
  def toScalaRange                  = toInterval.toScalaRange
  def toIndexed: Direct[Long]       = toInterval.toIndexed
  def toInt: Int                    = checkValidInt(value)
  def toLong: Long                  = value
  def toOption: Option[Long]        = if (isError) None else Some(toInt)
  def toInfo: Precise               = if (isError) fail(s"Cannot translate erroneous size") else Precise(this)

  @inline def foreachIndex(f: Index => Unit): Unit = toInterval foreach f
  def containsIndex(index: Index): Boolean = 0 <= index && index < value
  def lastIndex: Index = if (value <= 0) fail("empty.last") else value - 1

  override def toString = if (isError) "<no size>" else s"$value"
}

// Size is^Wshould be its own unapply (value class bugs drove us out for now)
object Size {
  implicit def sizeToSizeInfo(s: Size): SizeInfo = s.toInfo

  final val NoSize = new Size(-1)
  final val Zero   = new Size(0)
  final val One    = new Size(1)
  final val Two    = new Size(2)
  final val Three  = new Size(3)
  final val Four   = new Size(4)
  final val Five   = new Size(5)

  def apply(n: Long): Size           = if (n <= 0) Zero else new Size(n)
  def unapply(s: Size): Option[Long] = s.toOption

  private def fail(msg: String): Nothing = throw new ArithmeticException(msg)
}
