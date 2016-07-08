package klikatech._3_week

import math._
import java.util.Objects
import scala.runtime.ScalaRunTime

class Point(val x: Double, val y: Double) extends Ordered[Point] {
  import Point._

  // 4) сравнение точек;
  // не указано как. сравниваем по расстоянию от начала координат
  def compare(that: Point) = {
    val res = dist(Centre) - that.dist(Centre)
    if (res > 0) 1 else if (res < 0) -1 else 0
  }

  // 3) расстояние между точками;
  def dist(that: Point) = sqrt(pow((x - that.x), 2) + pow((y - that.y), 2))

  // 2) перемещение точки на произвольный вектор;
  def move(vect: Point) = Point(x + vect.x, y + vect.y)

  // 5) в какой координатной четверти лежит точка;
  lazy val quarter = {
    if (x >= 0) {
      if (y >= 0) 1 else 2
    } else {
      if (y >= 0) 4 else 3
    }
  }

  // 6) являются ли две точки симметричными относительно начала отсчёта;
  def symmetricTo(that: Point) = x == -that.x && y == -that.y

  // 7) являются ли три точки коллинеарными (т.е. лежащими на одной прямой).
  def collinearTo(p1: Point, p2: Point) = {
    val div = x / y
    (p1.x / p1.y) == div && (p2.x / p2.y) == div
  }

  // 1) toString, equals, hashCode;
  override def toString = s"Point($x, $y)"

  override def equals(other: Any) = other match {
    case that: Point => x == that.x && y == that.y
    case _ => false
  }

  override def hashCode = 41 * (41 + x.hashCode()) + y.hashCode()
}

object Point {
  val Centre = Point(0, 0)

  def apply(x: Double, y: Double) = new Point(x, y)

  def main(args: Array[String]): Unit = {
    println("comparison...")
    println(Point(7, 8) > Point(-11, -9))
    println(Point(7, 8) < Point(-11, -9))
    println(Point(7, 8) >= Point(-11, -9))
    println(Point(7, 8) <= Point(-11, -9))

    println("collinearTo...")
    println(Point(1, 2).collinearTo(Point(2, 4), Point(-3, -6)))
    println(Point(1, 2).collinearTo(Point(1, 4), Point(-3, -6)))

    println("quarter...")
    println(Point(1, 2).quarter)
    println(Point(-1, 2).quarter)
  }
}