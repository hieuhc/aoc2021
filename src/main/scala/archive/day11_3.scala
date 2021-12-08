package archive

import scala.annotation.tailrec
import scala.io.Source

object day11_3 {
  type pos = (Int, Int)
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day11.txt").getLines().toList
    val inputMap = input.zipWithIndex.flatMap { case (line: String, row: Int) =>
      line.zipWithIndex.map { case (ch: Char, col: Int) => ((row, col), ch) }
    }.toMap
    val nRows = input.length
    val nCols = input.head.length

    def mkDirMap(occupiedMap: Map[pos, Char], initPos: Seq[pos])(dirFn: pos => pos): Map[pos, Char] = {
      initPos.flatMap { case (r: Int, c: Int) =>
        expand((r, c))(dirFn).scanLeft(((-1, -1), '.')) { case (prev: (pos, Char), (row: Int, col: Int)) =>
          ((row, col), if (occupiedMap.getOrElse(prev._1, '.') == '.') prev._2 else occupiedMap(prev._1))
        }
      }.toMap
    }

    def expand(init: pos)(dirFn: pos => pos): List[pos] = {
      def helper(crr: pos): List[pos] =
        if (0 <= crr._1 && crr._1 < nRows && 0 <= crr._2 && crr._2 < nCols) crr :: helper(dirFn(crr)) else Nil

      helper(init)
    }

    def populate(occupiedMap: Map[pos, Char]) = {
      val northPos     = (0 until nCols).map((0, _))
      val southPos     = (0 until nCols).map((nRows - 1, _))
      val westPos      = (0 until nRows).map((_, 0))
      val eastPos      = (0 until nRows).map((_, nCols - 1))
      val northMap     = mkDirMap(occupiedMap, northPos)(pos => (pos._1 + 1, pos._2))
      val southMap     = mkDirMap(occupiedMap, southPos)(pos => (pos._1 - 1, pos._2))
      val westMap      = mkDirMap(occupiedMap, westPos)(pos => (pos._1, pos._2 + 1))
      val eastMap      = mkDirMap(occupiedMap, eastPos)(pos => (pos._1, pos._2 - 1))
      val southEastMap = mkDirMap(occupiedMap, northPos ++ westPos)(pos => (pos._1 + 1, pos._2 + 1))
      val northWestMap = mkDirMap(occupiedMap, southPos ++ eastPos)(pos => (pos._1 - 1, pos._2 - 1))
      val northEastMap = mkDirMap(occupiedMap, southPos ++ westPos)(pos => (pos._1 - 1, pos._2 + 1))
      val southWestMap = mkDirMap(occupiedMap, northPos ++ eastPos)(pos => (pos._1 + 1, pos._2 - 1))
      List(northMap, southMap, westMap, eastMap, southEastMap, northEastMap, northWestMap, southWestMap)
    }

    def iterate(initMat: Map[pos, Char]): (Map[pos, Char], Boolean) = {
      var isChange                      = false
      val listMap: List[Map[pos, Char]] = populate(initMat)
      val updatedMat = for (i <- input.indices;
        j <- input.head.indices) yield {
        val numOcc: Int = listMap.map(m => m(i, j)).count(b => b == '#')
        val crrSeat     = initMat(i, j)
        val seat =
          if (crrSeat == '#' && numOcc >= 5) 'L'
          else if (crrSeat == 'L' && numOcc == 0) '#'
          else initMat(i, j)
        if (seat != crrSeat) isChange = true
        ((i, j), seat)
      }
      (updatedMat.toMap, isChange)
    }

    @tailrec
    def sol(mat: Map[pos, Char], isChange: Boolean): Int = {
      if (!isChange) mat.count(t => t._2 == '#')
      else {
        val iterRes = iterate(mat)
        sol(iterRes._1, iterRes._2)
      }
    }
    println(sol(inputMap, isChange = true))
  }
}
