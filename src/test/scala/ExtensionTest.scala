import org.scalatest.{FlatSpec,Matchers}
import bitwise._

class ExtensionTest extends FlatSpec with Matchers {
  "zero extension" should "be success" in {
    val a = 10.toBit(4)
    val b = 5.toBit(4)

    assert(a.zeroExt(8) ==& 10.toBit(8))
    assert(b.zeroExt(8) ==& 5.toBit(8))
  }

  "zero extension" should "be failure" in {
    val a = 10.toBit(4)

    an[IllegalArgumentException] should be thrownBy { a.zeroExt(0) }
    an[IllegalArgumentException] should be thrownBy { a.zeroExt(-1) }
    an[IllegalArgumentException] should be thrownBy { a.zeroExt(4) }
    an[IllegalArgumentException] should be thrownBy { a.zeroExt(3) }
  }

  "sign extension" should "be success" in {
    val a = 10.toBit(4)
    val b = 5.toBit(4)

    assert(a.signExt(5) == 26.toBit(8))
    assert(a.signExt(8) ==& 0xfa.toBit(8))
    assert(b.signExt(8) ==& 5.toBit(8))
  }

  "sign extension" should "be failure" in {
    val a = 10.toBit(4)

    an[IllegalArgumentException] should be thrownBy { a.signExt(0) }
    an[IllegalArgumentException] should be thrownBy { a.signExt(-1) }
    an[IllegalArgumentException] should be thrownBy { a.signExt(3) }
    an[IllegalArgumentException] should be thrownBy { a.signExt(4) }
  }
}
