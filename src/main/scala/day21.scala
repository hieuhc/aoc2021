import scala.annotation.tailrec
object day21 {
  def main(args: Array[String]): Unit = {
    def calStep(s: Long): Long = s % 10 match {
      case 0 => 10
      case n => n
    }
    val stepList: List[Long] = (for {
      d1 <- 1 to 3
      d2 <- 1 to 3
      d3 <- 1 to 3
    } yield (d1 + d2 + d3).toLong).toList
    @tailrec
    def deterministic(diceCount: Long, firstPos: Long, firstScore: Long, secondPos: Long, secondScore: Long): Long = {
      if (firstScore >= 1000 && secondScore < 1000)
        secondScore * diceCount * 3
      else if (firstScore < 1000 && secondScore >= 1000)
        firstScore * diceCount * 3
      else {
        val nextCount = diceCount + 1
        val moveDist  = List(nextCount * 3 - 2, nextCount * 3 - 1, nextCount * 3).map(_ % 100).sum
        nextCount % 2 match {
          case 1 =>
            deterministic(
              nextCount,
              calStep(firstPos + moveDist),
              firstScore + calStep(firstPos + moveDist),
              secondPos,
              secondScore)
          case 0 =>
            deterministic(
              nextCount,
              firstPos,
              firstScore,
              calStep(secondPos + moveDist),
              secondScore + calStep(secondPos + moveDist))
        }
      }
    }
    println(deterministic(0L, 1L, 0L, 10L, 0L))

    def move(
        state: Map[(Long, Long), Long],
        win: Long,
        otherState: Map[(Long, Long), Long]): (Map[(Long, Long), Long], Long) = {

      val updatedState: Map[(Long, Long), Long] = state.toList.flatMap { case ((step, score), count) =>
        stepList.map(d => calStep(d + step)).map(newStep => ((newStep, score + newStep), count))
      }.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).sum) }

      val newState = updatedState.filterNot(_._1._2 > 20)
      val newWIn   = win + updatedState.filter(_._1._2 > 20).values.sum * otherState.values.sum
      (newState, newWIn)
    }

    def dirac(
        turnNo: Long,
        firstState: Map[(Long, Long), Long],
        firstWin: Long,
        secondState: Map[(Long, Long), Long],
        secondWin: Long): Long = {
      if (firstState.isEmpty || secondState.isEmpty)
        List(firstWin, secondWin).max
      else {
        if (turnNo % 2 == 1) {
          val (newFirstState, newFirstWin) = move(firstState, firstWin, secondState)
          dirac(turnNo + 1, newFirstState, newFirstWin, secondState, secondWin)
        } else {
          val (newSecState, newSecWin) = move(secondState, secondWin, firstState)
          dirac(turnNo + 1, firstState, firstWin, newSecState, newSecWin)
        }
      }
    }
    println(dirac(1, Map((1L, 0L) -> 1L), 0L, Map((10L, 0L) -> 1L), 0L))

  }

}
