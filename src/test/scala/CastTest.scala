import org.scalatest.{FlatSpec,Matchers}
import bitwise._

class CastTest extends FlatSpec with Matchers {
  "unsigned long cast" should "be success" in {
    assert(10.toBit().toULong == 10)
    assert(10.toBit(5).toULong == 10)
    assert(10.toBit(10).toULong == 10)
    assert((-1).toBit(4).toULong == 15)
    assert((-1).toBit().toULong == 3)
  }

  "signed long cast" should "be success" in {
    assert(10.toBit().toSLong == -6)
    assert(10.toBit(5).toSLong == 10)
    assert((-1).toBit().toSLong == -1)
    assert((-10).toBit().toSLong == -10)
    assert((-1).toBit(64).toSLong == -1)
    assert((-10).toBit(64).toSLong == -10)
  }

  "unsigned long cast" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(65).toULong }
    an[IllegalArgumentException] should be thrownBy{ (-1).toBit(65).toULong }
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(64).toULong }
    an[IllegalArgumentException] should be thrownBy{ (-1).toBit(64).toULong }
  }

  "signed long cast" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(65).toSLong }
    an[IllegalArgumentException] should be thrownBy{ (-1).toBit(65).toSLong }
  }

  "unsigned int cast" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(32).toUInt }
  }

  "signed int cast" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(33).toSInt }
    an[IllegalArgumentException] should be thrownBy{ (-1).toBit(33).toSInt }
  }

  "unsigned short cast" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(16).toUShort }
  }

  "signed short cast" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy{ 10.toBit(17).toSShort }
    an[IllegalArgumentException] should be thrownBy{ (-1).toBit(17).toSShort }
  }

  "Long.toBit" should "be success" in {
    val r = new scala.util.Random(1)

    for (_ <- 0 to 1000) {
      val length = 1 + r.nextInt(62)
      val value: Long = 1L << (length - 1L)

      val b = value.toBit(length)

      assert(b.length == length)
      assert(b.value == value)
    }
  }

  "Int.toBit" should "be success" in {
    val r = new scala.util.Random(1)

    for (_ <- 0 to 1000) {
      val length = 1 + r.nextInt(30)
      val value: Int = 1 << (length - 1)

      val b = value.toBit(length)

      assert(b.length == length)
      assert(b.value == value)
    }
  }

  "Boolean.toBit" should "be success" in {
    val r = new scala.util.Random(1)

    for(_ <- 0 to 1000) {
      val length = r.nextInt(Integer.MAX_VALUE)
      val value = r.nextInt(2) == 1

      val b = value.toBit(length)

      assert(b.length == length)
      assert(b.value == (if(value) 1 else 0))
    }
  }
}
