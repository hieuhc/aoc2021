import scala.io.Source

object day1 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("input_day1.txt").getLines().map(_.toInt).toList

    def countIncrease(list: List[Int]): Int = list.tail.zip(list.dropRight(1)).count { case (a1, a2) => a1 > a2 }
    println(countIncrease(numbers))

    val x = numbers.zip(numbers.tail).zip(numbers.tail.tail).map { case ((a, b), c) => a + b + c }
    println(countIncrease(x))

  }

}
