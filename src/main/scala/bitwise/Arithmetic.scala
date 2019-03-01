package bitwise

trait Arithmetic[A, B] extends Add[A, B] with Sub[A, B] with Mul[A, B] with Div[A, B]

trait Add[A, B] {
  def +(that: A): B
  def +&(that: A): B
}

trait Sub[A, B] {
  def -(that: A): B
  def -&(that: A): B
}

trait Mul[A, B] {
  def *(that: A): B
  def *&(that: A): B
}

trait Div[A, B] {
  def /(that: A): B
}