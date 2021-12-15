import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

object day15 {
  def main(args: Array[String]): Unit = {
    type Pos     = (Int, Int)
    type RiskMap = Map[Pos, Int]
    def posMove(p: Pos, d: Pos): Pos = (p._1 + d._1, p._2 + d._2)

    val input: List[String] = Source.fromResource("input_day15.txt").getLines().toList
    val initMap: RiskMap = input.indices.foldLeft(Map[Pos, Int]()) { case (crrM, y) =>
      val str = input(y)
      crrM ++ str.indices.map(x => ((x, y), str(x).asDigit)).toMap
    }
    val first: Pos = (0, 0)
    val initSize   = input.length

    val newMap: RiskMap = (for {
      x <- 0 until initSize * 5
      y <- 0 until initSize * 5
    } yield (x, y)).map { case (x, y) =>
      if (initMap.contains((x, y))) ((x, y), initMap((x, y)))
      else {
        val initV = initMap((x % initSize, y % initSize))
        val newV = (initV + x / initSize + y / initSize) % 9 match {
          case 0   => 9
          case num => num
        }
        ((x, y), newV)
      }
    }.toMap

    def nav2(map: RiskMap, muState: MutableMap[Pos, Int])(candidates: RiskMap): Unit = {
      if (candidates.nonEmpty) {
        muState ++= candidates
        val tmp: List[(Pos, Int)] = candidates.toList.flatMap { case (pos, v) =>
          List((-1, 0), (1, 0), (0, -1), (0, 1)).map(pd => posMove(pos, pd)).filter(p =>
            map.contains(p) && (muState.getOrElse(p, Int.MaxValue) >= (v + map(p)))).map(p => (p, v + map(p)))
        }
        val newC: RiskMap = tmp.groupBy(_._1).map { case (p, v) => (p, v.map(_._2).min) }
        nav2(map, muState)(newC)
      }
    }
    var muMap1 = MutableMap[Pos, Int]()
    nav2(initMap, muMap1)(Map[Pos, Int](first -> newMap(first)))
    val initLast: Pos = (initSize - 1, initSize - 1)
    println("Solution 1:", muMap1(initLast) - muMap1(first))

    var muMap2 = MutableMap[Pos, Int]()
    nav2(newMap, muMap2)(Map[Pos, Int](first -> newMap(first)))
    val last: Pos = (initSize * 5 - 1, initSize * 5 - 1)
    println("Solution 2:", muMap2(last) - muMap2(first))

  }

}
