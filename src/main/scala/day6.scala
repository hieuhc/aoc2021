import scala.io.Source
object day6 {
  def main(array: Array[String]): Unit = {
    val input: List[Int]          = Source.fromResource("input_day6.txt").getLines().toList.head.split(",").map(_.toInt).toList
    val initState: Map[Int, Long] = input.map((_, 1)).groupBy(_._1).map { case (k, lst) => (k, lst.length) }
    def calculateFish(time: Int): Map[Int, Long] = (1 to time).foldLeft(initState) { case (crrState, _) =>
      val updatedState = crrState.map[Int, Long] { case (k, v) => (k - 1, v) }
      val res = updatedState ++ Map(
        -1 -> 0L,
        8  -> updatedState.getOrElse(-1, 0L),
        6  -> (updatedState.getOrElse(6, 0L) + updatedState.getOrElse(-1, 0L)))
      res
    }
    println(calculateFish(80).values.sum)
    println(calculateFish(256).values.sum)
  }

}
