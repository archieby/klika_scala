package klikatech._4_week

object Functions {

  /**
   * 1. Higher order functions: Напишите прокси-генератор: функцию, которая принимает на вход две функции: f и g и возвращает
   * функцию, которая применяет g к результату выполнения f и возвращает результат выполнения f.
   */
  def proxyGenerator[T, R](f: T => R, g: R => Unit) = (t: T) => {
    val res = f(t)
    g(res)
    res
  }

  /**
   * Alternative implementation
   */
  def proxyGenerator2[T, R](g: R => Unit)(f: T => R) = (t: T) => {
    val res = f(t)
    g(res)
    res
  }

  /**
   * 2. Function literal: Напишите функцию, котороя принимает на вход список натуральных чисел и возвращает список,
   * содержащий элементы первого списка возведённые в квадрат. Использование циклов не допускается.
   */
  def squaredSeq(seq: Seq[Int]) = seq.map(x => x * x)

  /**
   * 3. Partially applied functions: Используя прокси-генератор из первого задания напишите функцию-обёртку, выводящую в
   * консоль результаты выполнения любой функции.
   */
  def loggingWrapper[T, R] = proxyGenerator(_: T => R, println)

  /**
   * Alternative implementation
   */
  def loggingWrapper2[T, R] = proxyGenerator2[T, R](println) _

  /**
   * 4. Partial functions: Напишите функцию, вычисляющую квадратный корень любого неотрицательного числа.
   */
  val sqrt: PartialFunction[Double, Double] = {
    case d if d > -1 => math.sqrt(d)
  }

  def main(args: Array[String]): Unit = {
    val res = loggingWrapper[Int, Int](_ * 2)(4)
    println(res)
    val res2 = loggingWrapper2[Int, Boolean](_ > 7)(4)
    println(res2)

    println(squaredSeq(Seq(1, 2, 3, 4, 5, 6)))

    println(sqrt(4))
    println(sqrt(-1))
  }
}