package klikatech._5_week

sealed trait Tree[+T]

case object Empty extends Tree[Nothing]
case class Node[T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

object Tree {
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Empty => Empty
    case Node(value, left, right) => Node(f(value), map(left)(f), map(right)(f))
  }

  def toList[A](t: Tree[A]): List[A] = t match {
    case Empty => throw new UnsupportedOperationException
    case Node(value, left, right) => {
      fold(t, List[A]()) { (l, el, r) =>
        l ::: (el :: r)
      }
    }
  }

  def reduce[A](t: Tree[A])(f: (A, A) => A): A = t match {
    case Empty => throw new UnsupportedOperationException
    case Node(value, left, right) => {
      fold(t, value) { (l, el, r) =>
        f(f(l, el), r)
      }
    }
  }

  def fold[A, B](tree: Tree[A], acc: B)(f: (B, A, B) => B): B = tree match {
    case Empty => acc
    case Node(value, left, right) => f(fold(left, acc)(f), value, fold(right, acc)(f))
  }
}

object Trees {
  import Tree._
  def main(args: Array[String]): Unit = {
    val tr1 = Node(2, Node(2, Node(1, Empty, Empty), Node(4, Empty, Empty)), Node(7, Empty, Node(3, Empty, Empty)))
    println(reduce(tr1) {
      (a, b) => a + b
    })
    println(toList(tr1))
    println(toList(map(tr1)(_ * 4)))
  }
}