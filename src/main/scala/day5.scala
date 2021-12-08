import scala.io.Source
import math.min
import math.max

object day5 {
  object int {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
  def updateDict[T](crrMap: Map[T, Int], updateMap: Map[T, Int]): Map[T, Int] = {
    updateMap ++ crrMap.map { case (k, v) =>
      if (updateMap.contains(k)) (k, v + updateMap(k)) else (k, v)
    }
  }

  def main(array: Array[String]): Unit = {
    val input = Source.fromResource("input_day5.txt").getLines()
    val map = input.foldLeft(Map[(Int, Int), Int]()) { case (crrMap, line) =>
      line match {
        case s"${int(x1)},${int(y1)} -> ${int(x2)},${int(y2)}" =>
          if (x1 == x2) {
            val updateMap: Map[(Int, Int), Int] = (min(y1, y2) to max(y1, y2)).map(y => ((x1, y), 1)).toMap
            updateDict[(Int, Int)](updateMap, crrMap)
          } else if (y1 == y2) {
            val updateMap: Map[(Int, Int), Int] = (min(x1, x2) to max(x1, x2)).map(x => ((x, y1), 1)).toMap
            updateDict[(Int, Int)](updateMap, crrMap)
          } else {
            val updateMap = (0 to math.abs(x1 - x2)).map(i =>
              ((x1 + (x2 - x1) * i / math.abs(x1 - x2), y1 + (y2 - y1) * i / math.abs(y1 - y2)), 1)).toMap
            updateDict[(Int, Int)](updateMap, crrMap)

          }
      }

    }
    println(map.count(_._2 > 1))

  }

}
