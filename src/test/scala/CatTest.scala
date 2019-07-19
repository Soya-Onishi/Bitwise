import org.scalatest.{Matchers, FlatSpec}
import bitwise._

class CatTest extends FlatSpec with Matchers {
  def getMax(length: Int): Int = 1 << (length - 1)

  "cat test" should "be success" in {
    val r1 = new scala.util.Random(1)

    for (_ <- 0 to 1000) {
      val aLen = 1 + r1.nextInt(30)
      val bLen = 1 + r1.nextInt(30)
      val a = r1.nextInt(getMax(aLen))
      val b = r1.nextInt(getMax(bLen))

      val aBit = a.toBit(aLen)
      val bBit = b.toBit(bLen)
      val cBit = aBit ++ bBit

      val aBig = BigInt(a)
      val bBig = BigInt(b)
      val cBig = (aBig << bLen) | bBig

      assert(cBit.length == (aLen + bLen))
      assert(cBit.value == cBig)
    }
  }
}
