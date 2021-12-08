package archive

import scala.io.Source

object day6 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input_day6.txt").getLines().toList
    def aggregate(initSet: Set[Char], aggrFunc: (Set[Char], Set[Char]) => Set[Char]): Int =
      (lines ++ List("")).foldLeft((initSet, 0)) {
        case ((curSet: Set[Char], res: Int), line: String) =>
          if (line == "") {
            (initSet, res + curSet.size)
          } else {
            (aggrFunc(curSet, line.toSet), res)
          }
      }._2
    println(aggregate(Set[Char](), _ | _))
    println(aggregate(s"abcdefghijklmnopqrstuvwxyz".toSet, _ & _))
  }

}
