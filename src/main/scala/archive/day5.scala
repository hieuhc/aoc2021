package archive

import scala.io.Source

object day5 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input_day5.txt").getLines().toList
    def navigate(navString: String, range: Int, lowerChar: Char): Int =
      navString.foldLeft((0, range)) { case ((min: Int, max: Int), nav: Char) =>
        if (nav == lowerChar) {
          (min, (min + max) / 2)
        } else {
          (((min + max) / 2.0).ceil.toInt, max)
        }
      }._1
    val allSeats = lines.map(seatString => {
      val row = navigate(seatString.slice(0, 7), 127, 'F')
      val num = navigate(seatString.slice(7, 10), 7, 'L')
      row * 8 + num
    })
    println(allSeats.max)
    println((allSeats.min to allSeats.max).sum - allSeats.sum)
  }

}
