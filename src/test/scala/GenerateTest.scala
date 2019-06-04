import bitwise._
import org.scalatest.{FlatSpec, Matchers}

class GenerateTest extends FlatSpec with Matchers {
  "Bit generate" should "be success" in {
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

  "Bit generate" should "be failure" in {
    an[IllegalArgumentException] should be thrownBy { 1.toBit(0) }
    an[IllegalArgumentException] should be thrownBy { 0.toBit(0) }
    an[IllegalArgumentException] should be thrownBy { 2.toBit(1) }
    an[IllegalArgumentException] should be thrownBy { (-1).toBit(1) }
    an[IllegalArgumentException] should be thrownBy { (-1).toBit(0) }
    an[IllegalArgumentException] should be thrownBy { 1.toBit(-1) }
    an[IllegalArgumentException] should be thrownBy { (-1).toBit(-1) }
  }
}
