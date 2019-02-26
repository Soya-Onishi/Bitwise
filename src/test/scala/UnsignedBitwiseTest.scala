import org.scalatest._
import bitwise._

class UnsignedBitwiseTest extends FlatSpec with Matchers {
  "BitwiseTest" should "appropriate initialize" in {
    noException should be thrownBy 10.toUBit(4)
    noException should be thrownBy 10.toUBit(10)

    noException should be thrownBy "0b0101".toUBit
    noException should be thrownBy "0b1010".toUBit(4)

    noException should be thrownBy "0o573".toUBit

    noException should be thrownBy "0xabcd".toUBit
    noException should be thrownBy "0x7bcd".toUBit(15)
  }

  it should "appropriate length" in {
    assert(1.toUBit.length == 1)
    assert(10.toUBit.length == 4)

    assert("0xabcd".toUBit.length == 16)

  }

  it should "raise IllegalArgumentException" in {
    an[IllegalArgumentException] should be thrownBy (-1).toUBit
    an[IllegalArgumentException] should be thrownBy 10.toUBit(3)

    an[IllegalArgumentException] should be thrownBy "0xabcd".toUBit(10)

    val bit = 10.toUBit(4)

    an[IllegalArgumentException] should be thrownBy bit(4, 2)
    an[IllegalArgumentException] should be thrownBy bit(2, -1)
    an[IllegalArgumentException] should be thrownBy bit(2, 3)
    an[IllegalArgumentException] should be thrownBy bit(4)
    an[IllegalArgumentException] should be thrownBy bit(-1)

    an[IllegalArgumentException] should be thrownBy bit.pad(-1)
    an[IllegalArgumentException] should be thrownBy bit.pad(0)

    an[IllegalArgumentException] should be thrownBy bit.tail(-1)
    an[IllegalArgumentException] should be thrownBy bit.tail(5)

    an[IllegalArgumentException] should be thrownBy bit.head(-1)
    an[IllegalArgumentException] should be thrownBy bit.head(5)
  }

  it should "raise NumberFormatException" in {
    an[NumberFormatException] should be thrownBy "0o9ab".toUBit
    an[NumberFormatException] should be thrownBy "".toUBit
    an[NumberFormatException] should be thrownBy "0x".toUBit
  }

  it should "get appropriate operation result" in {
    assert(1.toUBit + 2.toUBit == 3.toUBit)
    assert((1.toUBit + 2.toUBit).length == 2)
    assert((1.toUBit +& 2.toUBit).length == 3)

    assert(("0xFFFF".toUBit & 1.toUBit) == 1.toUBit)
    assert(("0xFFFF".toUBit & "0x0F0F".toUBit) == "0x0F0F".toUBit)
    assert(("0xFFFF".toUBit & "0x00F0F".toUBit).length == 20)
    assert(("0x0FFFF".toUBit & "0x0F0F".toUBit).length == 20)

    assert(("0x00FF".toUBit | "0xFF00".toUBit) == "0xFFFF".toUBit)
    assert(("0x00FF".toUBit | "0xFF00".toUBit).length == 16)
    assert(("0x00FF".toUBit | "0x0FF00".toUBit).length == 20)
    assert(("0x000FF".toUBit | "0xFF00".toUBit).length == 20)

    assert(("0xFFFF".toUBit ^ "0xFFFF".toUBit) == 0.toUBit)
    assert(("0xF0F0".toUBit ^ "0x0F0F".toUBit) == "0xFFFF".toUBit)
    assert(("0xFFFF".toUBit ^ "0x0FFFF".toUBit).length == 20)
    assert(("0x0FFFF".toUBit ^ "0xFFFF".toUBit).length == 20)

    assert(("0xFFFF".toUBit == "0xFFFF".toUBit) == true)
    assert(("0xFFFF".toUBit == "0x0FFF".toUBit) == false)

    assert(("0xFFFF".toUBit != "0x0FFF".toUBit) == true)
    assert(("0xFFFF".toUBit != "0xFFFF".toUBit) == false)

    assert(("0xFFFF".toUBit > "0x0FFF".toUBit) == true)
    assert(("0xFFFF".toUBit > "0xFFFF".toUBit) == false)
    assert(("0xFFFF".toUBit > "0x0FFFF".toUBit) == false)
    assert(("0x0FFF".toUBit > "0xFFFF".toUBit) == false)

    assert(("0xFFFF".toUBit >= "0x0FFF".toUBit) == true)
    assert(("0xFFFF".toUBit >= "0xFFFF".toUBit) == true)
    assert(("0xFFFF".toUBit >= "0x0FFFF".toUBit) == true)
    assert(("0x0FFF".toUBit >= "0xFFFF".toUBit) == false)

    assert(("0xFFFF".toUBit < "0x0FFF".toUBit) == false)
    assert(("0xFFFF".toUBit < "0xFFFF".toUBit) == false)
    assert(("0xFFFF".toUBit < "0x0FFFF".toUBit) == false)
    assert(("0x0FFF".toUBit < "0xFFFF".toUBit) == true)

    assert(("0xFFFF".toUBit <= "0x0FFF".toUBit) == false)
    assert(("0xFFFF".toUBit <= "0xFFFF".toUBit) == true)
    assert(("0xFFFF".toUBit <= "0x0FFFF".toUBit) == true)
    assert(("0x0FFF".toUBit <= "0xFFFF".toUBit) == true)
  }

  it should "get appropriate string" in {
    assert("0xFF".toUBit.toString == "11111111")
    assert("0xF".toUBit(8).toString == "00001111")
  }

  it should "get appropriate padding" in {
    assert(0.toUBit.pad(3).toString == "000")
    assert("0xF".toUBit.pad(5).toString == "01111")
    assert("0xFFFF".toUBit.pad(8).toString == "11111111")
  }
}
