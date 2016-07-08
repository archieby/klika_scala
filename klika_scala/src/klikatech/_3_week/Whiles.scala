package klikatech._3_week

import scala.util.Random

object Whiles {
  def whileLoop(cond: => Boolean)(op: => Unit): Unit = {
    if (cond) {
      op
      whileLoop(cond)(op)
    }
  }

  def run[T](body: => T): Untillable[T] = new Untillable(body)

  // Just for the sake of using Structural Types. Plus run() method's return type is hidden from the caller
  def run2[T](body: => T): { def until(f: T => Boolean): Unit } = {
    new {
      def until(f: T => Boolean): Unit = {
        if (!f(body)) {
          run(body).until(f)
        }
      }
    }
  }

  // Looks like I've got the task wrong :). Another run to the bunch
  def run3[T](body: => T): Filterable[T] = new Filterable(body)

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
    println("testing while...")
    var i = 10
    whileLoop(i > -1) {
      println(i)
      i -= 1
    }

    println("testing run...")
    run {
      val rnd = Random.nextInt(10)
      println(rnd)
      rnd
    } until (_ == 0)

    println("testing run2...")
    run2 {
      val rnd = Random.nextInt(10)
      println(rnd)
      rnd
    } until (_ == 0)

    println("testing run3...")
    var value = run3 { Random.nextInt(10) } until (_ == 0)
    println(value)
    whileLoop(value != None) {
      value = run3 { Random.nextInt(10) } until (_ == 0)
      println(value)
    }
  }

}

