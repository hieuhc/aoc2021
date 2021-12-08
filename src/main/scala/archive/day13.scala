package archive

import scala.annotation.tailrec
import scala.io.Source

object day13 {
  object long {
    def unapply(arg: String): Option[Long] = arg.toLongOption
  }
  def main(args: Array[String]): Unit = {
    val input    = Source.fromResource("input_day13.txt").getLines().toList
    val est: Int = input.head.toInt
    val busSeq: Seq[(Long, Long)] =
      input.tail.head.split(",").zipWithIndex.filterNot(_._1 == "x").map { case (long(bus), offset) =>
        (bus, -offset.toLong)
      }.toSeq
    val (waitMin, earliestInd) = busSeq.map { case (num, _) =>
      if (est % num == 0) 0 else num - est % num
    }.zipWithIndex.min
    println(busSeq(earliestInd)._1 * waitMin)

    def solve(equation1: (Long, Long), equation2: (Long, Long)): (Long, Long) = {
      val (coef1, const1) = equation1
      val (coef2, const2) = equation2
      val gcdCoef         = greatestCommonDivisor(coef1, coef2)
      val m: Long         = coef2 / gcdCoef
      val n: Long         = (0L until coef2).find(p => (coef1 * p + const1 - const2) % coef2 == 0).get
      (coef1 * m, coef1 * n + const1)
    }

    @tailrec
    def greatestCommonDivisor(a: Long, b: Long): Long = b match {
      case 0 => a
      case _ => greatestCommonDivisor(b, a % b)
    }

    println(busSeq.reduce(solve)._2)
  }

}
