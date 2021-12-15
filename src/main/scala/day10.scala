import scala.io.Source
object day10 {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day10.txt").getLines().toList
    def findCorrupted(str: String): (List[Char], Int) = {
      val finalStack = str.foldLeft(List[Char]()) { case (crrStack, crrChar) =>
        val (expected: Char, score: Int) = crrChar match {
          case '>' => ('<', 25137)
          case '}' => ('{', 1197)
          case ']' => ('[', 57)
          case ')' => ('(', 3)
          case _   => ('_', 0)
        }
        if (expected == '_') crrChar :: crrStack
        else if (expected != crrStack.head)
          return (List(), score)
        else {
          crrStack.tail
        }
      }
      (finalStack, 0)
    }
    val wrongClosingMap = input.map(findCorrupted)
    println(wrongClosingMap.map(_._2).sum)
    val inComplete = wrongClosingMap.map(_._1).filter(_.nonEmpty)
    val scores: List[Long] = inComplete.map(str => {
      str.foldLeft(0L) { case (point, ch) =>
        ch match {
          case '(' => point * 5 + 1
          case '[' => point * 5 + 2
          case '{' => point * 5 + 3
          case '<' => point * 5 + 4
        }
      }
    }).sorted
    println(scores(scores.length / 2))
  }

}
