import scala.io.Source
object day7 {
  def main(array: Array[String]): Unit = {
    val input = Source.fromResource("input_day7.txt").getLines().toList.head.split(",").map(_.toLong)
    def calculateSteps(f: (Long, Long) => Long): Long = {
      (input.min to input.max).foldLeft(Long.MaxValue) { case (crrSum, crrN) =>
        val updatedSum = input.map(k => f(crrN, k)).sum
        if (updatedSum < crrSum) updatedSum else crrSum
      }
    }
    println(calculateSteps((a, b) => math.abs(a - b)))
    println(calculateSteps((a, b) => math.abs(a - b) * (math.abs(a - b) + 1) / 2))
  }

}
