package klikatech._2_week

class Random private (val seed: Long) {
  import Random._
  def nextInt = {
    val nextSeed = (seed * multiplier + addend) & mask;
    ((nextSeed >>> 12), Random(nextSeed))
  }

  override def toString = s"Random[$seed]"
}

object Random {
  private val multiplier = 0x5DEECE66DL;
  private val addend = 0xBL;
  private val mask = (1L << 48) - 1;

  private def apply(seed: Long) = new Random(seed)
  def apply() = new Random(System.currentTimeMillis())

  def main(args: Array[String]): Unit = {
    val r1 = Random()
    println(r1.nextInt)
    println(r1.nextInt)
    val (x2, r2) = r1.nextInt
    println(r2.nextInt)
  }
}