package archive

object day15 {
  def main(args: Array[String]): Unit = {
    val input   = List(0, 13, 1, 16, 6, 17)
    val initMap = input.slice(0, input.length - 1).zipWithIndex.map { case (k, v) => (k, v + 1) }.toMap
    def solve(numTurn: Int): Int = {
      (input.length + 1 to numTurn).foldLeft((initMap, input.last)) {
        case ((mem: Map[Int, Int], lastNum: Int), turn: Int) =>
          val crrNum: Int = if (!mem.contains(lastNum)) 0 else turn - 1 - mem(lastNum)
          (mem + (lastNum -> (turn - 1)), crrNum)
      }._2
    }
    println(solve(2020))
    println(solve(30000000))
  }
}
