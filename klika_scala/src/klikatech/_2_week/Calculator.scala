package klikatech._2_week

trait Num[T] {

  private[_2_week] def value: T

  def /(other: Num[T]): Num[T]

  def +(other: Num[T]): Num[T]

  def -(other: Num[T]): Num[T]

  def *(other: Num[T]): Num[T]

  def -:(other: Num[T]): Num[T] = other - this
  def /:(other: Num[T]): Num[T] = other / this
  def +:(other: Num[T]): Num[T] = other + this
  def *:(other: Num[T]): Num[T] = other * this
}

case class I(private[_2_week] val value: Int) extends Num[Int] {
  def +(other: Num[Int]) = Num(value + other.value)
  def -(other: Num[Int]) = Num(value - other.value)
  def *(other: Num[Int]) = Num(value * other.value)
  def /(other: Num[Int]) = Num(value / other.value)
}

case class D(private[_2_week] val value: Double) extends Num[Double] {
  def +(other: Num[Double]) = Num(value + other.value)
  def -(other: Num[Double]) = Num(value - other.value)
  def *(other: Num[Double]) = Num(value * other.value)
  def /(other: Num[Double]) = Num(value / other.value)
}

object Num {
  implicit def itod(i: I): D = D(i.value)

  def apply(d: Double) = new D(d)
  def apply(i: Int) = new I(i)
}

object Calculator extends App {
  println(Num(2) *: Num(4.2))
  println(Num(2) * Num(4))

  println(Num(4) / Num(2))
  println(Num(4.4) / Num(2))

  println(Num(4) /: Num(2))
  println(Num(4.4) /: Num(2))

  println(Num(4) /: Num(2))
  println(Num(4.4) /: Num(2))

  println(Num(7) - Num(4) /: Num(2) + Num(3))
  println(Num(7) - Num(4.3) * Num(2) -: Num(4.3) * Num(2) + Num(3))
  println(Num(7) *: Num(4.1) /: Num(2) + Num(3))
  println(Num(7) - Num(4) /: Num(2) + Num(3))
  println(Num(7) -: Num(4) / Num(2) + Num(3))

  //в чем разница между операциями  +:  и  *: - В приоритете
}

