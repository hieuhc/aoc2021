package archive

import scala.annotation.tailrec
import scala.io.Source

object day8 {
  object int {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input_day8.txt").getLines().toList
    val program = input.map { case s"${operation} ${int(argument)}" =>
      val realStep = if (operation != "jmp") 1 else argument
      val nopStep  = if (operation == "jmp") 1 else argument
      val acc      = if (operation != "acc") 0 else argument
      (operation, realStep, nopStep, acc)
    }

    @tailrec
    def loop(lineIdx: Int, acc: Int, visited: Set[Int]): Int = {
      val (_, step, _, accVal) = program(lineIdx)
      if (visited.contains(lineIdx)) acc
      else loop(lineIdx + step, acc + accVal, visited ++ Set(lineIdx))
    }
    println(loop(0, 0, Set()))

    def explore(lineIdx: Int, isModified: Boolean, acc: Int, visited: Set[Int]): Option[Int] = {
      val (operation, step, nopStep, accVal) = program(lineIdx)
      if (visited.contains(lineIdx))
        None
      else if (lineIdx == program.length - 1) {
        Some(acc + accVal)
      } else {
        (if (!isModified && (operation == "nop" || operation == "jmp"))
           explore(lineIdx + nopStep, isModified = true, acc, visited ++ Set(lineIdx))
         else None) match {
          case Some(res) => Some(res)
          case None =>
            explore(lineIdx + step, isModified, acc + accVal, visited ++ Set(lineIdx))
        }
      }
    }
    println(explore(0, false, 0, Set()).get)
  }
}
