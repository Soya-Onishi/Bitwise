package bitwise

class FromIntToBit(val from: BigInt) {
  def toUBit(): UBit = UBit(from)
  def toUBit(width: Int): UBit = UBit(from, width)
}

class FromStringToBit(v: String) {
  val (value, length) = getBigInt(v)

  private def getBigInt(s: String): (BigInt, Int) = {
    val head = s.slice(0, 2)
    val body =
      if(s.length > 2)
        s.substring(2)
      else
        ""

    s.slice(0, 2) match {
      case "0b" => (BigInt(body, 2), body.length)
      case "0o" => (BigInt(body, 8), body.length * 3)
      case "0x" => (BigInt(body, 16), body.length * 4)
      case _ =>
        val v = BigInt(s)
        (v, v.bitLength)
    }
  }

  def toUBit(): UBit = UBit(value, length)
  def toUBit(width: Int): UBit = UBit(value, width)
}
