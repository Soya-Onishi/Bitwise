package bitwise.internal

import scala.math.max

abstract class Bit(var value: BigInt, val length: Int) {
  type BitType <: Bit

  def apply(pos: Int): UBit = {
    require(pos >= 0 && pos < length, s"pos[$pos] must be between 0 to ${length - 1}")

    do_apply(pos, pos)
  }

  def apply(to: Int, from: Int): UBit = {
    require(to >= from, s"to[$to] must be greater or equal than from[$from]")
    require(from >= 0, s"to[$to] and from[$from] must be positive or zero")
    require(to < length, s"to[$to] and from[$from] must be less than length[$length]")

    do_apply(to, from)
  }

  protected def do_apply(to: Int, from: Int): UBit = {
    val length = to - from + 1
    val range = (1 << length) - 1
    val shifted = value >> from

    UBit(shifted & range, length)
  }

  def pad[T <: BitType](length: Int)(implicit cbf: BitBuilder[T]): T = {
    require(length > 0, s"length[$length] must be positive")
    do_pad(length)
  }

  protected def do_pad[T <: BitType](length: Int)(implicit cbf: BitBuilder[T]): T

  def tail(n: Int): UBit = {
    require(n < length && n >= 0, s"n[$n] must be between 0 to ${length - 1}")
    do_tail(n)
  }

  protected def do_tail(n: Int): UBit = do_apply(length - n - 1, 0)

  def head(n: Int): UBit = {
    require(n <= length && n > 0, s"n[$n] must be between 1 to $length")
    do_head(n)
  }

  private def do_head(n: Int): UBit = do_apply(length - 1, length - n - 1)

  def update(pos: Int, value: Boolean): Unit = {
    if(pos >= length || pos < 0)
      throw new IndexOutOfBoundsException(s"pos[$pos] must be between 0 to ${length - 1}")

    do_update(pos, value)
  }

  protected def do_update(pos: Int, isSet: Boolean): Unit = {
    val mask = BigInt(1) << pos

    value =
      if (isSet)
        value | mask
      else
        value & ~mask
  }

  def +[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _))(_ + _)
  def +&[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _) + 1)(_ + _)
  def -[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _))(_ - _)
  def -&[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _) + 1)(_ - _)

  protected def do_calc[T <: BitType](that: T)(decideLength: (Int, Int) => Int)(op: (BigInt, BigInt) => BigInt)(implicit cbf: BitBuilder[T]): T = {
    val length = decideLength(this.length, that.length)
    val mask = (BigInt(1) << length) - 1
    val value = op(this.value, that.value) & mask
    cbf(value, length)
  }

  def ==[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): Boolean = this.value == that.value
  def !=[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): Boolean = this.value != that.value
  def <[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): Boolean = this.value < that.value
  def <=[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): Boolean = this.value <= that.value
  def >[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): Boolean = this.value > that.value
  def >=[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): Boolean = this.value >= that.value

  def toBool(): Boolean = {
    require(length == 1, s"length[$length] must be 1")
    value == 1
  }

  def toBools(): Seq[Boolean] = {
    (0 until length).map{ x => ((value >> x) & 1) == 1 }.reverse
  }
}

object UBit {
  def apply(x: BigInt): UBit = {
    elementMustBePositiveOrZero(x)

    new UBit(x, x.bitLength)
  }

  def apply(x: BigInt, length: Int): UBit = {
    elementMustBePositiveOrZero(x)
    require(x.bitLength <= length, s"length[$length] must be longer or equal than x length[${x.bitLength}]")

    new UBit(x, length)
  }

  private def elementMustBePositiveOrZero(x: BigInt): Unit = {
    require(x >= 0, s"x[$x] must be zero or positive.")
  }
}

class UBit private(value: BigInt, length: Int) extends Bit(value, length) {
  type BitType = UBit

  protected def do_pad[T <: BitType](length: Int)(implicit cbf: BitBuilder[T]): T = {
    if (length >= this.length)
      cbf(value, length)
    else {
      val v = apply(length - 1, 0)
      cbf(v.value, v.length)
    }
  }

  override def toString: String = {
    val raw = value.toString(2)
    val pad = "0" * (length - raw.length)

    pad + raw
  }

  def &[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _))(_ & _)
  def |[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _))(_ | _)
  def ^[T <: BitType](that: T)(implicit cbf: BitBuilder[T]): T = do_calc(that)(max(_, _))(_ ^ _)
}
