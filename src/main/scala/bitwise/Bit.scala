package bitwise

import scala.math.max

object Bit {
  private def measureLength(value: BigInt): Int = {
    def countLeastLength(value: BigInt, first: Boolean = true): Int = {
      if(value == 0)
        if(first) { 1 } else { 0 }
      else
        1 + countLeastLength(value >> 1, first=false)
    }

    val absValue =
      if(value < 0)
        -value
      else
        value

    if(value < 0)
      countLeastLength(absValue) + 1
    else
      countLeastLength(absValue)
  }

  def apply(value: BigInt): Bit = {
    apply(value, measureLength(value))
  }

  def apply(value: BigInt, width: Int): Bit = {
    require(width > 0, s"width[$width] must be greater than 0")

    val leastWidth = measureLength(value)
    require(width >= leastWidth, s"value[$value]'s width[$width] must be at least $leastWidth")

    new Bit(value, width)
  }
}

class Bit(val value: BigInt, val length: Int) {
  def apply(pos: Int): Bit = {
    require(pos >= 0 && pos < length, s"pos[$pos] must be between 0 to ${length - 1}")

    this.apply(pos, pos)
  }

  def apply(to: Int, from: Int): Bit = {
    require(to >= from, s"to[$to] must be greater or equal than from[$from]")
    require(from >= 0, s"to[$to] and from[$from] must be positive or zero")
    require(to < length, s"to[$to] and from[$from] must be less than length[$length]")

    val truncatedValue = (this.value & ((1 << (to + 1)) - 1)) >> from
    val newLength = to - from + 1

    new Bit(truncatedValue, newLength)
  }

  def +(that: Bit): Bit = {
    val newLength = max(this.length, that.length)
    val newValue = (this.value + that.value) & ((1 << newLength) - 1)

    new Bit(newValue, newLength)
  }

  def -(that: Bit): Bit = {
    val newValue = this.value + that.value
    val newLength = max(this.length, that.length)

    new Bit(newValue, newLength)
  }

  def lsb(n: Int): Bit = {
    require(n <= length && n > 0, s"n[$n] must be between 1 to ${length - 1}")

    this.apply(n - 1, 0)
  }

  def msb(n: Int): Bit = {
    require(n <= length && n > 0, s"n[$n] must be between 1 and ${length - 1}")

    this.apply(length - 1, length - n)
  }

  def zeroExt(n: Int): Bit = {
    require(n > length, s"n[$n] must be larger than lenght[$length]")

    new Bit(this.value, n)
  }

  def signExt(n: Int): Bit = {
    require(n > length, s"n[$n] must be larger than lenght[$length]")

    val extender =
      if(msb(1) == 1.toBit())
        ((BigInt(1) << n) - 1) ^ ((BigInt(1) << length) - 1)
      else {
        BigInt(0)
      }

    new Bit(value | extender, n)
  }

  def ==(that: Bit): Boolean = {
    this.value == that.value
  }

  def !=(that: Bit): Boolean = {
    this.value != that.value
  }

  def >(that: Bit): Boolean = {
    this.value > that.value
  }

  def >=(that: Bit): Boolean = {
    this.value >= that.value
  }

  def <(that: Bit): Boolean = {
    this.value < that.value
  }

  def <=(that: Bit): Boolean = {
    this.value <= that.value
  }

  def ==&(that: Bit): Boolean = {
    (this.value == that.value) && (this.length == that.length)
  }

  def !=&(that: Bit): Boolean = {
    (this.value != that.value) || (this.length != that.length)
  }

  def toULong: Long = {
    require(length < 64, s"bit length[$length] should be less than 64")

    val mask = (BigInt(1) << length) - 1
    (value & mask).toLong
  }

  def toSLong: Long = {
    require(length <= 64, s"bit length[$length] should be less than or equal 64")

    if (this.msb(1) == 1.toBit()) {
      val mask = -1L ^ ((1L << length) - 1)
      (value | mask).toLong
    } else {
      value.toLong
    }
  }

  def toUInt: Int = {
    require(length < 32, s"bit length[$length] should be less than 32")
    this.toULong.toInt
  }

  def toSInt: Int = {
    require(length <= 32, s"bit length[$length] should be less than or equal 32")
    this.toSLong.toInt
  }

  def toUShort: Short = {
    require(length < 16, s"bit length[$length] should be less than 16")
    this.toULong.toShort
  }

  def toSShort: Short = {
    require(length <= 16, s"bit length[$length] should be less than or equal 16")
    this.toSLong.toShort
  }

  override def toString: String = s"$value<$length>"
}
/*
abstract class bitwise.Bit(var value: BigInt, val length: Int) {
  type BitType <: bitwise.Bit

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

  def pad(length: Int): BitType = {
    require(length > 0, s"length[$length] must be positive")
    do_pad(length)
  }

  protected def do_pad(length: Int): BitType

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

  def +(that: BitType): BitType
  def +&(that: BitType): BitType

  def ==(that: BitType): Boolean = this.value == that.value
  def !=(that: BitType): Boolean = this.value != that.value
  def <(that: BitType): Boolean = this.value < that.value
  def <=(that: BitType): Boolean = this.value <= that.value
  def >(that: BitType): Boolean = this.value > that.value
  def >=(that: BitType): Boolean = this.value >= that.value

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

class UBit private(value: BigInt, length: Int) extends bitwise.Bit(value, length) {
  type BitType = UBit

  def do_pad(length: Int): BitType = {
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

  override def +(that: BitType): BitType = {
    val sum = this +& that
    sum.tail(1)
  }

  override def +&(that: BitType): BitType = {
    val length = max(this.length, that.length)
    val sum = this.value + that.value

    new UBit(sum, length + 1)
  }

  def &(that: BitType): BitType = {
    val length = max(this.length, that.length)

    new UBit(this.value & that.value, length)
  }

  def |(that: BitType): BitType = {
    val length = max(this.length, that.length)

    new UBit(this.value | that.value, length)
  }

  def ^(that: BitType): BitType = {
    val length = max(this.length, that.length)

    new UBit(this.value ^ that.value, length)
  }
}
*/