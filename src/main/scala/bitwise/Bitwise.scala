import bitwise.internal._
import scala.language.implicitConversions

package object bitwise {
  implicit def fromIntTransformer(n: Int): FromIntToBit = new FromIntToBit(n)
  implicit def fromStringTransformer(s: String): FromStringToBit = new FromStringToBit(s)


  type UBit = bitwise.internal.UBit
}
