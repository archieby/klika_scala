package klikatech._5_week

import scala.annotation.tailrec

object Factorial extends App {
  def calculateFactorial(i: Int) = {
    require(i > 0)
    @tailrec
    def go(j: Int, acc: Int): Int = j match {
      case 1 => acc
      case x => go(j - 1, acc * j)
    }
    go(i, 1)
  }

  println(calculateFactorial(5))
  println(calculateFactorial(10))
  println(calculateFactorial(8))
  println(calculateFactorial(3))
  println(calculateFactorial(1))
  //  println(calculateFactorial(-1))
}