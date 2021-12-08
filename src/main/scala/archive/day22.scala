package archive

import scala.io.Source

object day22 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromResource("input_day22.txt").getLines().toSeq
    val splitLineIdx       = input.indices.find(input(_) == "").get
    val deck1              = input.slice(1, splitLineIdx).map(_.toLong)
    val deck2              = input.slice(splitLineIdx + 2, input.length).map(_.toLong)
    def play(player1: Seq[Long], player2: Seq[Long]): Seq[Long] = {
      if (player1.isEmpty || player2.isEmpty) player1 ++ player2
      else {
        val first1 = player1.head
        val first2 = player2.head
        if (first1 > first2) play(player1.drop(1) ++ Seq(first1, first2), player2.drop(1))
        else if (first1 < first2) play(player1.drop(1), player2.drop(1) ++ Seq(first2, first1))
        else throw new Exception("Equal deck")
      }
    }
    val winnerDeck = play(deck1, deck2)
    val sol1       = (winnerDeck.length to 1 by -1).zip(winnerDeck).map(tup => tup._1.toLong * tup._2).sum
    println(sol1)

    def recursiveCombat(
        firstP: Seq[Long],
        secondP: Seq[Long],
        gameState: Set[(Seq[Long], Seq[Long])]): (Boolean, Seq[Long]) = {
      if (gameState.contains((firstP, secondP))) (true, firstP ++ secondP)
      else {
        if (firstP.isEmpty) (false, secondP)
        else if (secondP.isEmpty) (true, firstP)
        else {
          val firstPHead  = firstP.head
          val secondPHead = secondP.head
          val isFirstWinThisRound =
            if (firstPHead <= firstP.length - 1 && secondPHead <= secondP.length - 1)
              recursiveCombat(firstP.tail.take(firstPHead.toInt), secondP.tail.take(secondPHead.toInt), Set())._1
            else if (firstPHead > secondPHead) true
            else false
          val updatedState = gameState ++ Set((firstP, secondP))
          if (isFirstWinThisRound)
            recursiveCombat(firstP.drop(1) ++ Seq(firstPHead, secondPHead), secondP.drop(1), updatedState)
          else
            recursiveCombat(firstP.drop(1), secondP.drop(1) ++ Seq(secondPHead, firstPHead), updatedState)
        }
      }
    }
    val recursiveCombatWinnerDeck = recursiveCombat(deck1, deck2, Set())._2
    println((recursiveCombatWinnerDeck.length to 1 by -1).zip(recursiveCombatWinnerDeck).map(t =>
      t._1.toLong * t._2).sum)
  }

}
