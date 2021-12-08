package archive

import scala.io.Source

object day10 {
  def main(args: Array[String]): Unit = {
    val input: Set[Int] = Source.fromResource("input_day10.txt").getLines().toSet.map { a: String => a.toInt }
    val maxAdapter      = input.max
    def sol1(crr: Int, remain: Set[Int], numOne: Int, numThree: Int): Option[Int] = {
      if (remain.isEmpty) {
        crr match {
          case x if x < 4 => Some((numOne + 1 / x) * (numThree + (x - 1) / 2))
          case _          => None
        }
      } else {
        (1 to 3).filter(i => remain.contains(crr - i)).foldLeft(None: Option[Int]) {
          case (prev: Option[Int], diff: Int) =>
            prev match {
              case Some(a) => Some(a)
              case None    => sol1(crr - diff, remain.diff(Set(crr - diff)), numOne + 1 / diff, numThree + (diff - 1) / 2)
            }
        }
      }
    }
    val f1 = if (input.contains(1)) 1L else 0L
    val f2 = if (input.contains(2)) 1L + f1 else 0L
    val f3 = if (input.contains(3)) 1L + f2 + f1 else 0L
    val sol2 = (4 to maxAdapter).foldLeft((f1, f2, f3)) { case ((m1: Long, m2: Long, m3: Long), num) =>
      if (!input.contains(num)) (m2, m3, 0L) else (m2, m3, m1 + m2 + m3)
    }
    println(sol1(maxAdapter, input.diff(Set(maxAdapter)), 0, 1).get)
    println(sol2._3)

    def p1(input: List[Long]) = {
      val (dif1, dif2, dif3, _) = {
        input.foldLeft((0L, 0L, 0L, 0L)) {
          case ((dif1C, dif2C, dif3C, last), c) if (c - last == 1) => (dif1C + 1, dif2C, dif3C, c)
          case ((dif1C, dif2C, dif3C, last), c) if (c - last == 2) => (dif1C, dif2C + 1, dif3C, c)
          case ((dif1c, dif2c, dif3c, last), c) if (c - last == 3) => (dif1c, dif2c, dif3c + 1, c)
        }
      }
      println(dif1, dif2, dif3)
      dif1 * (dif3 + 1)
    }
    println(p1(input.toList.sorted.map(_.toLong)))
  }

}
