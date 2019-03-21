package bitwise

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

  def setLength(length: Int): BitType = {
    if (length <= 0)
      throw new IndexOutOfBoundsException(s"length[$length] must be between 1 to ${length - 1}")

    do_setLength(length)
  }

  protected def do_setLength(length: Int): BitType

  def tail(n: Int): UBit = {
    require(n < length && n >= 0, s"n[$n] must be between 0 to ${length - 1}")
    do_tail(n)
  }

  protected def do_tail(n: Int): UBit = do_apply(length - n - 1, 0)

  def head(n: Int): UBit = {
    require(n <= length && n > 0, s"n[$n] must be between 1 to $length")
    do_head(n)
  }

  protected def do_head(n: Int): UBit = do_apply(length - 1, length - n - 1)

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

  def ++(that: Bit): UBit = {
    val left = this.value << that.length
    val right = that.value

    UBit(left + right, this.length + that.length)
  }

  protected def do_calc[T <: Bit](that: T)(decideLength: (Int, Int) => Int)(op: (BigInt, BigInt) => BigInt)(implicit bf: BitBuilder[T]): T = {
    val length = decideLength(this.length, that.length)
    val mask = (BigInt(1) << length) - 1
    val value = op(this.value, that.value) & mask
    bf(value, length)
  }

  def +(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(max)(_ + _)
  def +&(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(max(_, _) + 1)(_ + _)

  def -(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(max)(_ - _)
  def -&(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(max(_, _) + 1)(_ - _)

  def *(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(max)(_ * _)
  def *&(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(_ + _)(_ * _)

  def /(that: BitType)(implicit bb: BitBuilder[BitType]): BitType = do_calc(that)(max)(_ / _)


  protected abstract def compare(x: BigInt, y: BigInt): BigInt
  def ==(that: BitType): Boolean = compare(this.value, that.value) == 0
  def !=(that: BitType): Boolean = compare(this.value, that.value) != 0
  def <(that: BitType): Boolean = compare(this.value, that.value) < 0
  def <=(that: BitType): Boolean = compare(this.value, that.value) <= 0
  def >(that: BitType): Boolean = compare(this.value, that.value) > 0
  def >=(that: BitType): Boolean = compare(this.value, that.value) >= 0

  def toBool: Boolean = {
    require(length == 1, s"length[$length] must be 1")
    value == 1
  }

  def toBools: Seq[Boolean] = {
    (0 until length).map{ x => ((value >> x) & 1) == 1 }
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

  protected def do_setLength(length: Int): BitType = {
    if (length >= this.length)
      new UBit(value, length)
    else
      apply(length - 1, 0)
  }


  override def toString: String = {
    val raw = value.toString(2)
    val pad = "0" * (length - raw.length)

    pad + raw
  }

  protected def compare(x: BigInt, y: BigInt): BigInt = x - y

  def &(that: BitType): BitType = do_calc(that)(max(_, _))(_ & _)
  def |(that: BitType): BitType = do_calc(that)(max(_, _))(_ | _)
  def ^(that: BitType): BitType = do_calc(that)(max(_, _))(_ ^ _)
}

//class SBit(value: BigInt, length: Int) extends Bit(value, length) {
//  type BitType = SBit
//
//  protected def do_setLength(length: Int): BitType = {
//    if(length >= this.length) {
//      if(this.head(1).toBool)
//        new SBit(-value & ((BigInt(1) << length) - 1), length)
//      else
//        new SBit(value, length)
//    } else {
//      new SBit(value & ((BigInt(1) << length) - 1), length)
//    }
//  }
//
//  protected def compare(x: BigInt, y: BigInt): BigInt = {
//
//  }
//}