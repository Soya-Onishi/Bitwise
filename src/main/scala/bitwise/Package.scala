import scala.language.implicitConversions

package object bitwise {
  implicit def fromIntTransformer(n: Int): FromIntToBit = new FromIntToBit(n)
  implicit def fromStringTransformer(s: String): FromStringToBit = new FromStringToBit(s)

  implicit val ubitCalculateUBit = new BitBuilder[UBit] {
    def apply(value: BigInt, length: Int): UBit = UBit(value, length)
  }
}
