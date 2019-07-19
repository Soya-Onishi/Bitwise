import scala.language.implicitConversions

package object bitwise {
  implicit def fromIntTransformer[T](n: T)(implicit ev: Int with Long with Boolean <:< T): FromIntegerToBit = n match {
    case bool: Boolean =>
      val v = if(bool) 1 else 0
      new FromIntegerToBit(BigInt(v))
    case num: Int => new FromIntegerToBit(BigInt(num))
    case num: Long => new FromIntegerToBit(BigInt(num))
  }
  // implicit def fromLongTransformer(n: Long): FromIntegerToBit = new FromIntegerToBit(BigInt(n))
  implicit def fromStringTransformer(s: String): FromStringToBit = new FromStringToBit(s)
}
