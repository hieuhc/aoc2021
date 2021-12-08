package archive

import scala.annotation.tailrec
import scala.io.Source

object day9 {
  object long {
    def unapply(arg: String): Option[Long] = arg.toLongOption
  }
  def main(args: Array[String]): Unit = {
    val input: List[Long] = Source.fromResource("input_day9.txt").getLines().toList.map { case long(s) => s }
    def twoSumExist(range: List[Long], sum: Long): Boolean = {
      @tailrec
      def helper(toMatch: Set[Long], crrIdx: Int): Boolean = {
        if (crrIdx == range.length) false
        else if (toMatch.contains(range(crrIdx))) true
        else helper(toMatch ++ Set(sum - range(crrIdx)), crrIdx + 1)
      }
      helper(Set(), 0)
    }
    val weakInd    = (25 until input.length) find (ind => !twoSumExist(input.slice(ind - 25, ind), input(ind)))
    val weakNumber = input(weakInd.get)
    println(weakNumber)

    @tailrec
    def spread(start: Int, end: Int, crrSum: Long): Long = {
      if (crrSum == weakNumber) {
        val range = input.slice(start, end + 1)
        range.min + range.max
      } else if (crrSum > weakNumber) spread(start + 1, end, crrSum - input(start))
      else spread(start, end + 1, crrSum + input(`end` + 1))
    }
    println(spread(0, 1, input.head + input(1)))
  }
}
