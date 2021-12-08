import scala.io.Source
object day4 {
  def main(array: Array[String]): Unit = {
    val lines: List[String] = Source.fromResource("day4_input.txt").getLines().toList
//    val lines: List[String]        = Source.fromResource("example_day4.txt").getLines().toList
    val drawNums: List[Int]        = lines.head.split(",").map(_.toInt).toList
    val drawNumsMap: Map[Int, Int] = drawNums.indices.map(idx => (drawNums(idx), idx)).toMap
    val boardToIdx: Map[List[Int], Int] = (2 until lines.length).foldLeft(Map[List[Int], Int](), List[List[Int]]()) {
      case ((boardMap, crrRows), idx) =>
        if (lines(idx) == "") {
          (boardMap, Nil)
        } else {
          val rowNum: List[Int] = lines(idx).split(" ").filterNot(_ == "").map(_.trim.toInt).toList
          val boardWithRows     = Map(rowNum -> ((idx - 1) / 6))
          val boardWithColumns =
            if (idx % 6 == 0) {
              val colNums: List[List[Int]] = (0 until 5).map(idx => {
                (crrRows :+ rowNum).map(_(idx))
              }).toList
              val colBoard = colNums.map((_, (idx - 1) / 6)).toMap
              colBoard
            } else Map()
          val updateBoard = boardMap ++ boardWithRows ++ boardWithColumns
          (updateBoard, crrRows :+ rowNum)
        }
    }._1
    val bingoScan: List[(Int, Int)] = boardToIdx.toList.map { case (lst, boardIdx) =>
      val minOccIdx = lst.map(drawNumsMap(_)).max
      (minOccIdx, boardIdx)
    }
//    val (lastDrawIdx, bingoBid): (Int, Int) = bingoScan.minBy(_._1)

    val boardToDrawWIn: Map[Int, Int] = bingoScan.groupBy(_._2).map { case (k, v: List[(Int, Int)]) =>
      (k, v.map(_._1).min)
    }
    val (bingoBid, lastDrawIdx): (Int, Int) = boardToDrawWIn.maxBy(_._2)

    val board: List[List[Int]] = boardToIdx.keys.filter(boardToIdx(_) == bingoBid).toList
    val sumUnmarked: Int = board.foldLeft(0) { case (crrS, lst) =>
      val sumOfUm = lst.filter(l => drawNumsMap(l) > lastDrawIdx).sum
      crrS + sumOfUm
    }
    println((sumUnmarked / 2) * drawNums(lastDrawIdx))

  }

}
