package archive

import scala.io.Source

object day3 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input_day3.txt").getLines.toList
    def traverse(down: Int, right: Int): Long = {
      (lines.indices by down).foldLeft(0) {
        case (numTree, row) =>
          val col    = (row * right / down) % lines.head.length
          val posVal = lines(row).charAt(col)
          if (posVal == '#') {
            numTree + 1
          } else {
            numTree
          }
      }
    }
    println(traverse(1, 3))
    println(traverse(1, 1) * traverse(1, 3) * traverse(1, 5) * traverse(1, 7) * traverse(2, 1))

  }

}
