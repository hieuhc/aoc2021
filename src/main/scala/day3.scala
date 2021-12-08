import scala.io.Source

object day3 {
  def main(array: Array[String]): Unit = {
    val lines = Source.fromResource("input_day3.txt").getLines().toList
    val nCol  = lines.head.length
    val nRow  = lines.length
    val x: List[Int] = lines.foldLeft(List.fill(nCol)(0)) { case (crrNum: List[Int], str) =>
      val intVal: List[Int] = str.toList.map(_.asDigit)
      crrNum.zip(intVal).map { case (a, b) => a + b }
    }
    val gammaStr   = (x.map(v => if (v < nRow / 2) 0 else 1)).mkString("")
    val epsilonStr = (x.map(v => if (v < nRow / 2) 1 else 0)).mkString("")
    val gamma      = Integer.parseInt(gammaStr, 2)
    val epsilon    = Integer.parseInt(epsilonStr, 2)
    println(gamma * epsilon)

    def reduce(input: List[String], pos: Int, oxyGen: Boolean): List[String] = {
      val nRow = input.length.toFloat
      val x = input.foldLeft(0) { case (crrNum, str) =>
        crrNum + str.charAt(pos).asDigit
      }
      val mostCommon = if (x < nRow / 2) 0 else 1
      val qualified  = if (oxyGen) mostCommon else 1 - mostCommon
      input.filter(_.charAt(pos).asDigit == qualified)
    }
    def generator(oxy: Boolean): String = {
      (0 until nCol).foldLeft(lines) { case (input, pos) =>
        if (input.length == 1)
          input
        else {
          reduce(input, pos, oxy)
        }
      }.head
    }
    val oxyVal: String = generator(true)
    val c02Val: String = generator(false)
    println(Integer.parseInt(oxyVal, 2) * Integer.parseInt(c02Val, 2))
  }
}
