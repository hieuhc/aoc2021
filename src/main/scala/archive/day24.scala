package archive

import scala.io.Source

object day24 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromResource("input_day24.txt").getLines().toList
    def parseDirection(s: String): List[String] = {
      if (s == "") Nil
      else {
        s match {
          case s"se${next}" => "se" :: parseDirection(next)
          case s"e${next}"  => "e" :: parseDirection(next)
          case s"ne${next}" => "ne" :: parseDirection(next)
          case s"sw${next}" => "sw" :: parseDirection(next)
          case s"w${next}"  => "w" :: parseDirection(next)
          case s"nw${next}" => "nw" :: parseDirection(next)
          case _            => throw new Exception(s"not found ${s}")
        }
      }
    }
    def move(crr: (Int, Int), dir: String): (Int, Int) = {
      val (x, y) = crr
      dir match {
        case "nw" => (x - 1, y - 1)
        case "w"  => (x - 2, y)
        case "sw" => (x - 1, y + 1)
        case "ne" => (x + 1, y - 1)
        case "e"  => (x + 2, y)
        case "se" => (x + 1, y + 1)
      }
    }

    def navigate(dirSeq: Seq[String]): (Int, Int) = {
      val res = dirSeq.foldLeft(((0, 0), 0)) { case ((crrPos, crrFlip), dir) =>
        val nextPos = move(crrPos, dir)
        if (nextPos == (0, 0)) (nextPos, crrFlip + 1) else (nextPos, crrFlip)
      }
      res._1
    }
    val flipPerTile: Map[(Int, Int), Int] =
      input.map(parseDirection).map(navigate).groupBy(identity).map(t => (t._1, t._2.length))
    println(flipPerTile.count(_._2 % 2 == 1))

    def flipPerDay(blackState: Map[(Int, Int), Boolean], crrD: Int): Map[(Int, Int), Boolean] = {
      if (crrD == 100)
        return blackState
//      println(s"day $crrD: ${countBlack(blackState)}")
      val allBlack     = blackState.filter(_._2)
      val xRange       = allBlack.keySet.map(_._1)
      val yRange       = allBlack.keySet.map(_._2)
      val (xMin, xMax) = (xRange.min, xRange.max)
      val (yMin, yMax) = (yRange.min, yRange.max)
      val updatedState = (for (x <- (xMin - 2 to xMax + 2);
        y <- (yMin - 2 to yMax + 2)) yield {
        val neighbor           = Seq("nw", "w", "sw", "ne", "e", "se").map(move((x, y), _))
        val countNeighBorBlack = neighbor.count(k => blackState.getOrElse(k, false))
        val crrVal             = blackState.getOrElse((x, y), false)
        val newVal =
          if (crrVal && (countNeighBorBlack == 0 || countNeighBorBlack > 2)) false
          else if (!crrVal && countNeighBorBlack == 2) true
          else crrVal
        ((x, y), newVal)
      }).toMap
      flipPerDay(blackState ++ updatedState, crrD + 1)
    }

    def countBlack(state: Map[(Int, Int), Boolean]) = state.count(_._2)
    val flipDayZero                                 = flipPerTile.map(t => (t._1, t._2 % 2 == 1))
    println(countBlack(flipPerDay(flipDayZero, 0)))
  }

}
