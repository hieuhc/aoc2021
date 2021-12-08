package archive

import scala.annotation.tailrec
import scala.io.Source

object day11 {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day11.txt").getLines().toList
    val nRows               = input.length
    val nCols               = input.head.length

    def crrState1(mat: List[String], row: Int, col: Int): String = {
      evalState(
        (for (i <- -1 to 1;
          j     <- -1 to 1) yield {
          if (row + i < 0 || row + i > nRows - 1 || col + j < 0 || col + j > nCols - 1)
            '.'
          else if (i == 0 && j == 0)
            '.'
          else mat(row + i)(col + j)
        }).toList,
        4)
    }

    def crrState2(mat: List[String], row: Int, col: Int): String = {
      evalState(
        for (rangeI <- List((row - 1 to 0 by -1).toList, List(-1), ((row + 1) until nRows).toList);
          rangeJ    <- List((col - 1 to 0 by -1).toList, List(-1), ((col + 1) until nCols).toList)) yield {
          val rangeI2 = if (rangeI == List(-1)) List.fill(rangeJ.length)(row) else rangeI
          val rangeJ2 = if (rangeJ == List(-1)) List.fill(rangeI.length)(col) else rangeJ
          rangeI2.zip(rangeJ2).find { case (i, j) => List('L', '#').contains(mat(i)(j)) } match {
            case None                         => '.'
            case Some(ch) if ch == (row, col) => '.'
            case Some(ch)                     => mat(ch._1)(ch._2)
          }
        },
        5
      )
    }
    def evalState(neighbors: List[Char], occupyThreshold: Int): String = {
      neighbors.foldLeft(0) { case (occ: Int, ch: Char) =>
        ch match {
          case '#' => occ + 1
          case _   => occ
        }
      } match {
        case 0                             => "empty"
        case occ if occ >= occupyThreshold => "occupied"
        case _                             => "No Change"
      }
    }
    def iterate(initMat: List[String])(stateFn: (List[String], Int, Int) => String): (List[String], Boolean) = {
      var isChange = false
      val updatedString = for (i <- input.indices;
        j <- input.head.indices) yield {
        val nextState = stateFn(initMat, i, j)
        val crrSeat   = initMat(i)(j)
        val seat =
          if (crrSeat == '#' && nextState == "occupied") 'L'
          else if (crrSeat == 'L' && nextState == "empty") '#'
          else initMat(i)(j)
        if (seat != crrSeat) isChange = true
        seat
      }
      (updatedString.grouped(nCols).map(lst => lst.mkString("")).toList, isChange)
    }

    @tailrec
    def sol(mat: List[String], isChange: Boolean)(stateFn: (List[String], Int, Int) => String): Int = {
      if (!isChange) mat.map(str => str.filter(_ == '#').length).sum
      else {
        val iterRes = iterate(mat)(stateFn)
        sol(iterRes._1, iterRes._2)(stateFn)
      }
    }
//    println(sol(input, isChange = true)(crrState1))
    val t0 = System.nanoTime()

    println(sol(input, isChange = true)(crrState2))
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")

  }

}
