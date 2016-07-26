package klikatech._5_week

object LogicFunctions extends App {
  def and(a: Boolean, b: Boolean) = a && b
  def or(a: Boolean, b: Boolean) = a || b
  def not(a: Boolean) = !a
  def then(a: Boolean, b: Boolean) = or(!a, b)

  def printTable(f: (Boolean, Boolean, Boolean) => Boolean) = {
    val boolVals = List(false, true)
    def getRow(cWidth: Int = 7)(ps: Any*) = {
      def srtRepr(x: Any) = {
        val str = x.toString()
        str + (" " * (cWidth - str.size))
      }
      ps.map(srtRepr).mkString
    }
    val header = getRow()("A", "B", "C", "Result")
    val rows = for {
      a <- boolVals
      b <- boolVals
      c <- boolVals
    } yield getRow()(a, b, c, f(a, b, c))
    (header +: rows).foreach(println)
  }

  println("and(a, and(b, c))")
  printTable((a: Boolean, b: Boolean, c: Boolean) => and(a, and(b, c)))
  println("\nthen(or(a, b), or(b, c))")
  printTable((a: Boolean, b: Boolean, c: Boolean) => then(or(a, b), or(b, c)))
  println("\nthen(or(a, b), or(b, c))")
  printTable((a: Boolean, b: Boolean, c: Boolean) => and(not(a), or(b, c)))
}