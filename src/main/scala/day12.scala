import scala.io.Source
object day12 {
  def main(array: Array[String]): Unit = {
    val input = Source.fromResource("input_day12.txt").getLines()
//    val input = Source.fromResource("example.txt").getLines()
    val map: Map[String, List[String]] = input.map { case s"${e1}-${e2}" =>
      (e1, e2)
    }.foldLeft(Map[
      String,
      List[String]]()) { case (crrMap, (e1, e2)) =>
      crrMap ++ Map(e1 -> (crrMap.getOrElse(e1, List()) :+ e2), e2 -> (crrMap.getOrElse(e2, List()) :+ e1))
    }
    println(map)
    def find(crrP: String, state: List[String], count: Int): Int = {
      if (crrP == "end") {
        count + 1
      } else {
        map(crrP).foldLeft(count) { case (crrCount, neighbor) =>
          if (neighbor.head.isLower && state.contains(neighbor))
            crrCount
          else
            find(neighbor, state :+ neighbor, crrCount)
        }
      }
    }
    println(find("start", List("start"), 0))

    def find2(crrP: String, path: List[String], state: Map[String, Int], isSmallVisited: Boolean, count: Int): Int = {
      if (crrP == "end") {
//        println(path)
        count + 1
      } else {
        map(crrP).foldLeft(count) { case (crrCount, neighbor) =>
          if (neighbor.head.isLower && state.contains(neighbor)) {
            if (isSmallVisited || neighbor == "start") crrCount
            else
              find2(neighbor, path :+ neighbor, state ++ Map(neighbor -> 2), isSmallVisited = true, crrCount)
          } else
            find2(
              neighbor,
              path :+ neighbor,
              state ++ Map(neighbor -> (state.getOrElse(neighbor, 0) + 1)),
              isSmallVisited,
              crrCount)
        }
      }
    }
    println(find2("start", List("start"), Map("start" -> 2), false, 0))
  }

}
