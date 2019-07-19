import bitwise._
import org.scalatest.{FlatSpec, Matchers}

class GenerateTest extends FlatSpec with Matchers {
  "Bit generate from Int" should "be success" in {
    noException should be thrownBy { 1.toBit() }
    noException should be thrownBy { 2.toBit() }
    noException should be thrownBy { 2.toBit(2) }
    noException should be thrownBy { (-1).toBit(2) }
    noException should be thrownBy { (-1).toBit() }
    noException should be thrownBy { (-2).toBit(3) }
    noException should be thrownBy { (-2).toBit() }
    noException should be thrownBy { 0.toBit() }
    noException should be thrownBy { 0.toBit(1) }
  }

  "Bit generate from Int" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy { 1.toBit(0) }
    an[IllegalArgumentException] should be thrownBy { 0.toBit(0) }
    an[IllegalArgumentException] should be thrownBy { 2.toBit(1) }
    an[IllegalArgumentException] should be thrownBy { (-1).toBit(1) }
    an[IllegalArgumentException] should be thrownBy { (-1).toBit(0) }
    an[IllegalArgumentException] should be thrownBy { 1.toBit(-1) }
    an[IllegalArgumentException] should be thrownBy { (-1).toBit(-1) }
  }

  "Bit generate from String" should "be success" in {
    val hex = "0x1001".toBit()
    assert(hex == 4097.toBit())
    assert(hex.length == 16)
    assert(hex.toUInt == 4097)

    val oct = "0o1001".toBit()
    assert(oct == 513.toBit())
    assert(oct.length == 12)
    assert(oct.toUInt == 513)

    val bin = "0b1001".toBit()
    assert(bin == 9.toBit())
    assert(bin.length == 4)
    assert(bin.toUInt == 9)
  }

  "Bit generate from String" should "be failure" in {
    an [IllegalArgumentException] should be thrownBy { "0101".toBit() }
    an [IllegalArgumentException] should be thrownBy { "0xg".toBit() }
    an [IllegalArgumentException] should be thrownBy { "0o9".toBit() }
    an [IllegalArgumentException] should be thrownBy { "0b20".toBit() }
  }
}
