import scala.io.Source

object day2 {
  def main(args: Array[String]): Unit = {
    val lines                        = Source.fromResource("input_day2.txt").getLines().toList
    val cmdList: List[(String, Int)] = lines.map { case s"${command} ${value}" => (command, value.toInt) }
    val (forward, depth): (Int, Int) = cmdList.foldLeft((0, 0)) { case ((x, y), (cmd, value)) =>
      cmd match {
        case "forward" => (x + value, y)
        case "up"      => (x, y - value)
        case "down"    => (x, y + value)
      }
    }
    println(forward * depth)
    val (forward2, _, depth2): (Long, Long, Long) =
      cmdList.foldLeft((0L, 0L, 0L)) { case ((x, aim, y), (cmd, value)) =>
        cmd match {
          case "forward" => (x + value, aim, y + aim * value)
          case "up"      => (x, aim - value, y)
          case "down"    => (x, aim + value, y)
        }
      }
    println(forward2 * depth2)
  }
}
