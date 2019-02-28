package bitwise.internal

trait BitBuilder[T <: Bit] {
  def apply(value: BigInt, length: Int): T
}
