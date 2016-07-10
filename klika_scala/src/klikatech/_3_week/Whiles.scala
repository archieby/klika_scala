package klikatech._3_week

import scala.util.Random

object Whiles {
  def whileLoop(cond: => Boolean)(op: => Unit): Unit = {
    if (cond) {
      op
      whileLoop(cond)(op)
    }
  }

  /**
   * 	Something like <p><code>do {...} while ()</code></p> construct in Java
   */
  def run[T](body: => T): Untillable[T] = new Untillable(body)

  /**
   * Same as the run(...) above. Just for the sake of using Structural Types. Plus run() method's return type is hidden from the caller
   */
  def run2[T](body: => T): { def until(f: T => Boolean): Unit } = {
    new {
      def until(f: T => Boolean): Unit = {
        if (!f(body)) {
          run(body).until(f)
        }
      }
    }
  }

  /**
   * Runs block once and returns Some in case its output corresponds to the provided predicate and None otherwise.
   */
  def run3[T](body: => T): Filterable[T] = new Filterable(body)

  /**
   * Hopefully the implementation that was requested.
   */
  def run4[T](body: => T): Untillable2[T] = new Untillable2(body)

  final class Untillable2[T](body: => T) {
    def until(predec: T => Boolean): T = {
      val bVal = body;
      if (!predec(bVal)) {
        run4(body).until(predec)
      } else {
        bVal
      }
    }
  }

  final class Untillable[T](body: => T) {
    def until(predec: T => Boolean): Unit = {
      if (!predec(body)) {
        run(body).until(predec)
      }
    }
  }

  final class Filterable[T](value: T) {
    def until(predec: T => Boolean) = {
      if (!predec(value)) {
        Some(value)
      } else {
        None
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("\ntesting while...")
    var i = 10
    whileLoop(i > -1) {
      println(i)
      i -= 1
    }

    println("\ntesting run...")
    run {
      val rnd = Random.nextInt(10)
      println(rnd)
      rnd
    } until (_ == 0)

    println("\ntesting run2...")
    run2 {
      val rnd = Random.nextInt(10)
      println(rnd)
      rnd
    } until (_ == 0)

    println("\ntesting run3...")
    var value = run3 { Random.nextInt(10) } until (_ == 0)
    println(value)
    whileLoop(value != None) {
      value = run3 { Random.nextInt(10) } until (_ == 0)
      println(value)
    }

    println("\ntesting run4...")
    println(run4 { Random.nextInt(10) } until (_ > 4))
  }

}

