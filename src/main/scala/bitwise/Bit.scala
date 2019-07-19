package bitwise

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

class Bit(val value: BigInt, val length: Int) extends Ordered[Bit] {
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

  def +(that: Bit): Bit = arithmetic(this, that)(_ + _)
  def -(that: Bit): Bit = arithmetic(this, that)(_ - _)
  def *(that: Bit): Bit = arithmetic(this, that)(_ * _)
  def /(that: Bit): Bit = arithmetic(this, that)(_ / _)

  private def arithmetic(x: Bit, y: Bit)(op: (BigInt, BigInt) => BigInt): Bit = {
    val newLength = scala.math.max(x.length, y.length)
    val newValue = op(x.value, y.value) & ((1 << newLength) - 1)

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
    require(n > length, s"n[$n] must be larger than length[$length]")

    new Bit(this.value, n)
  }

  def signExt(n: Int): Bit = {
    require(n > length, s"n[$n] must be larger than length[$length]")

    val extender =
      if(msb(1) == 1.toBit())
        ((BigInt(1) << n) - 1) ^ ((BigInt(1) << length) - 1)
      else {
        BigInt(0)
      }

    new Bit(value | extender, n)
  }

  def ++(that: Bit): Bit = {
    val newLength = this.length + that.length
    val newValue = (this.value << that.length) | that.value

    new Bit(newValue, newLength)
  }

  def ==(that: Bit): Boolean = {
    this.value == that.value
  }

  def !=(that: Bit): Boolean = {
    this.value != that.value
  }

  override def compare(that: Bit): Int = {
    if(this.value > that.value) 1
    else if (this.value == that.value) 0
    else -1
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