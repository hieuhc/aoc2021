import scala.annotation.tailrec
import scala.io.Source
object day9 {
  type Pos = (Int, Int)
  def posMove(p: Pos, d: Pos): Pos = (p._1 + d._1, p._2 + d._2)
  def main(array: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day9.txt").getLines().toList
    val map: Map[Pos, Int] = input.indices.foldLeft(Map[Pos, Int]()) { case (crrM, idx) =>
      val str                          = input(idx)
      val strMap: Map[(Int, Int), Int] = str.indices.map(x => ((x, idx), str(x).asDigit)).toMap
      crrM ++ strMap
    }
    val allPos: List[Pos] = (for {
      x <- input.head.indices
      y <- input.indices
    } yield (x, y)).toList
    val lowerPos: List[Pos] = allPos filter
      (p =>
        map(p) < List((-1, 0), (1, 0), (0, -1), (0, 1)).map(d => map.getOrElse(posMove(p, d), Integer.MAX_VALUE)).min)
    val riskValues = lowerPos.map(map(_) + 1).sum
    println(riskValues)

    @tailrec
    def findBasins(crrBasins: Set[Pos], newBasins: Set[Pos]): Set[Pos] = {
      if (newBasins.isEmpty) crrBasins
      else {
        val updatedCrrBasins = crrBasins ++ newBasins
        val updatedNewB = newBasins.foldLeft(Set[Pos]()) { case (crrSet, pos) =>
          crrSet ++ List((-1, 0), (1, 0), (0, -1), (0, 1)).map(posMove(pos, _)).filter(p =>
            map.contains(p) && map(p) < 9 && !updatedCrrBasins.contains(p))
        }
        findBasins(updatedCrrBasins, updatedNewB)
      }
    }
    println(lowerPos.map(p => findBasins(Set(), Set(p))).map(_.size).sortBy(_ * -1).take(3).product)
  }

}
