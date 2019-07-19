import scala.language.implicitConversions

package object bitwise {
  implicit def fromBooleanTransformer(b: Boolean): FromIntegerToBit = {
    val v = if(b) 1 else 0
    new FromIntegerToBit(BigInt(v))
  }

  implicit def fromIntTransformer(n: Int): FromIntegerToBit = new FromIntegerToBit(BigInt(n))
  implicit def fromLongTransformer(n: Long): FromIntegerToBit = new FromIntegerToBit(BigInt(n))
  implicit def fromStringTransformer(s: String): FromStringToBit = new FromStringToBit(s)
}
