import org.scalatest.{FlatSpec, Matchers}
import bitwise._

class TruncateTest extends FlatSpec with Matchers {
  "truncate" should "be success" in {
    val a = 10.toBit(8)
    val b = 5.toBit(8)
    assert(a(2, 0) == 2.toBit())
    assert(a(3, 0) == 10.toBit())
    assert(a(2, 1) == 1.toBit())
    assert(a(3, 1) == 5.toBit())
    assert(a(4, 0) == 10.toBit())
    assert(a(5, 1) == 5.toBit())
    assert(a(3) == 1.toBit())
    assert(a(0) == 0.toBit())
    assert(a(1) == 1.toBit())
    assert(b(0) == 1.toBit())
    assert(b(1) == 0.toBit())
    assert(a(4) == 0.toBit())
    assert(b(4) == 0.toBit())
  }

  "truncate" should "be failure" in {
    val a = 10.toBit(8)
    val b = 6.toBit(8)
    an[IllegalArgumentException] should be thrownBy { a(2, 3) }
    an[IllegalArgumentException] should be thrownBy { a(-1, 0) }
    an[IllegalArgumentException] should be thrownBy { a(2, -1) }
    an[IllegalArgumentException] should be thrownBy { a(30, 0) }
    an[IllegalArgumentException] should be thrownBy { a(8, 0) }
    an[IllegalArgumentException] should be thrownBy { a(-1, -1) }
    an[IllegalArgumentException] should be thrownBy { a(-100, -1) }
    an[IllegalArgumentException] should be thrownBy { a(-1) }
    an[IllegalArgumentException] should be thrownBy { a(30) }
  }

  "msb" should "be success" in {
    val a = 10.toBit(8)
    assert(a.msb(4) == 0.toBit())
    assert(a.msb(5) == 1.toBit())
    assert(a.msb(6) == 2.toBit())
    assert(a.msb(7) == 5.toBit())
    assert(a.msb(8) == 10.toBit())
  }

  "msb" should "be failure" in {
    val a = 10.toBit(8)
    an[IllegalArgumentException] should be thrownBy { a.msb(10) }
    an[IllegalArgumentException] should be thrownBy { a.msb(-1) }
    an[IllegalArgumentException] should be thrownBy { a.msb(0) }
  }

  "lsb" should "be success" in {
    val a = 10.toBit(8)
    assert(a.lsb(1) == 0.toBit())
    assert(a.lsb(2) == 2.toBit())
    assert(a.lsb(3) == 2.toBit())
    assert(a.lsb(4) == 10.toBit())
    assert(a.lsb(5) == 10.toBit())
    assert(a.lsb(8) == 10.toBit())
  }

  "lsb" should "be failure" in {
    val a = 10.toBit(8)
    an[IllegalArgumentException] should be thrownBy { a.lsb(10) }
    an[IllegalArgumentException] should be thrownBy { a.lsb(-1) }
    an[IllegalArgumentException] should be thrownBy { a.lsb(0) }
  }
}
