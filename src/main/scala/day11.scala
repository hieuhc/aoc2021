import scala.io.Source
object day11 {
  def main(args: Array[String]): Unit = {
    type Pos  = (Int, Int)
    type EMap = Map[Pos, Int]
    def posMove(p: Pos, d: Pos): Pos = (p._1 + d._1, p._2 + d._2)

    val input = Source.fromResource("input_day11.txt").getLines().toList
    val inMap: EMap = input.indices.foldLeft(Map[Pos, Int]()) { case (crrMap, idx) =>
      val str: String = input(idx)
      val strMap      = str.indices.map(i => (i, str(i).asDigit))
      crrMap ++ strMap.map { case (col, v) => ((idx, col), v) }
    }
    def step2(map: EMap, candidateLst: Iterable[Pos]): EMap = {
      if (candidateLst.isEmpty) map
      else {
        val newMap = candidateLst.foldLeft(map) { case (crrM, can) =>
          val canMap = List((-1, 0), (1, 0), (0, -1), (0, 1), (-1, 1), (1, -1), (-1, -1), (1, 1)).map(d =>
            posMove(can, d)).filter(p => crrM.contains(p) && crrM(p) != 0).map(p => (p, 1 + crrM(p))).toMap
          crrM ++ canMap ++ Map(can -> 0)
        }
        val newCandidateLst = newMap.keys.filter(newMap(_) > 9)
        step2(newMap, newCandidateLst)

      }
    }
    val numLoop = 100
    val res = (1 to numLoop).foldLeft((inMap, 0)) { case ((crrMap, count), _) =>
      val increaseOneMap: Map[Pos, Int] = crrMap.map { case (pos, v) => (pos, v + 1) }
      val step2Map                      = step2(increaseOneMap, increaseOneMap.keys.filter(increaseOneMap(_) > 9))
      (step2Map, count + step2Map.values.count(_ == 0))
    }
    println(res._2)

    def flash(map: EMap, num: Int): Int = {
      if (map.values.count(_ == 0) == map.size)
        num
      else {
        val increaseOneMap: Map[Pos, Int] = map.map { case (pos, v) => (pos, v + 1) }
        val step2Map                      = step2(increaseOneMap, increaseOneMap.keys.filter(increaseOneMap(_) > 9))
        flash(step2Map, num + 1)
      }
    }
    println(flash(inMap, 0))
  }

}
