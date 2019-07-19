import org.scalatest.{FlatSpec,Matchers}
import bitwise._

class CompareTest extends FlatSpec with Matchers {
  "compare" should "be success" in {
    assert(2.toBit() == 2.toBit())
    assert(2.toBit() == 2.toBit(10))

    assert(2.toBit() != 3.toBit())
    assert(2.toBit(2) != 3.toBit(2))
    assert(2.toBit() != 3.toBit(10))
    assert(1.toBit() != (-1).toBit())
    assert(1.toBit() != (-1).toBit(10))

    assert(2.toBit(2) ==& 2.toBit(2))
    assert(2.toBit() ==& 2.toBit(2))

    assert(2.toBit(2) !=& 2.toBit(3))
    assert(2.toBit(2) !=& 3.toBit(2))

    assert(2.toBit() < 3.toBit())
    assert(2.toBit(10) < 3.toBit())
    assert(2.toBit(10) < 3.toBit(30))
    assert((-1).toBit() < 3.toBit())
    assert((-1).toBit(10) < 3.toBit())
    assert((-1).toBit(10) < 3.toBit(30))

    assert(2.toBit() <= 3.toBit())
    assert(2.toBit() <= 2.toBit())
    assert((-1).toBit() <= 2.toBit())
    assert(2.toBit(10) <= 3.toBit())
    assert(2.toBit(10) <= 2.toBit())
    assert((-1).toBit(10) <= 2.toBit())
    assert(2.toBit(10) <= 3.toBit(30))
    assert(2.toBit(10) <= 2.toBit(30))
    assert((-1).toBit(10) <= 2.toBit(30))
  }
}
