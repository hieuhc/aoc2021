import scala.io.Source
object day13 {
  type Pos = (Int, Int)
  def posMove(p: Pos, d: Pos): Pos = (p._1 + d._1, p._2 + d._2)

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input_day13.txt").getLines().toList
    val (inMap, foldList) = input.foldLeft((Map[Pos, Int](), List[(String, Int)]())) { case ((crrMap, crrList), line) =>
      line match {
        case ""                      => (crrMap, crrList)
        case s"fold along ${k}=${v}" => (crrMap, crrList :+ (k, v.toInt))
        case s"${x},${y}"            => (crrMap ++ Map((x.toInt, y.toInt) -> 1), crrList)
      }
    }
    def foldX(map: Map[Pos, Int], xV: Int): Map[Pos, Int] = {
      val tmp = map.filter(_._1._1 < xV).map { case ((x, y), v) => ((xV - x - 1, y), v) }
      val tmp2 = tmp ++ map.filter(_._1._1 > xV).map {
        case ((x, y), v) =>
          val upP = (x - xV - 1, y)
          (upP, if (tmp.getOrElse(upP, 0) > v) tmp(upP) else v)
      }
      val xMax = tmp2.keys.map(_._1).max
      tmp2.map { case ((x, y), v) => ((xMax - x, y), v) }
    }
    def foldY(map: Map[Pos, Int], yV: Int): Map[Pos, Int] = {
      val tmp = map.filter(_._1._2 < yV).map { case ((x, y), v) => ((x, yV - y - 1), v) }
      val tmp2 = tmp ++ map.filter(_._1._2 > yV).map {
        case ((x, y), v) =>
          val upP = (x, y - yV - 1)
          (upP, if (tmp.getOrElse(upP, 0) > v) tmp(upP) else v)
      }
      val yMax = tmp2.keys.map(_._2).max
      tmp2.map { case ((x, y), v) => ((x, yMax - y), v) }
    }
    println(foldX(inMap, foldList.head._2).values.sum)
    val codeMap = foldList.foldLeft(inMap) { case (crrMap, (pos, v)) =>
      pos match {
        case "x" => foldX(crrMap, v)
        case "y" => foldY(crrMap, v)
      }
    }
    val xMax = codeMap.keys.map(_._1).max
    val xMin = codeMap.keys.map(_._1).min
    val yMax = codeMap.keys.map(_._2).max
    val yMin = codeMap.keys.map(_._2).min
    println(foldList)
    (yMin to yMax).foreach(y => {
      val p = (xMin to xMax).map(x => if (codeMap.contains((x, y))) "#" else ".").mkString(" ")
      println(p)
    })
  }

}
