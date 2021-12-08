package archive

import scala.io.Source

object day18 {
  object int {
    def unapply(arg: String): Option[Long] = arg.toLongOption
  }
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day18.txt").getLines().toList.map(_.replaceAll("\\s", ""))
    def eval(eq: String)(evalNoParen: String => Long): Long = {
      val eqNoParen =
        eq.foldLeft(("", "", 0)) { case ((primaryList: String, secondaryList: String, nParen: Int), ch: Char) =>
          if (nParen == 0) {
            ch match {
              case '(' => (primaryList, "(", 1)
              case _   => (primaryList :+ ch, secondaryList, 0)
            }
          } else ch match {
            case '(' => (primaryList, secondaryList :+ ch, nParen + 1)
            case ')' =>
              if (nParen > 1)
                (primaryList, secondaryList :+ ch, nParen - 1)
              else
                (primaryList + eval(secondaryList.drop(1))(evalNoParen), "", 0)
            case _ => (primaryList, secondaryList :+ ch, nParen)
          }
        }._1
      evalNoParen(eqNoParen)
    }
    def basicNoParen(eq: String): Long = {
      eq match {
        case s"${int(num)}" => num
        case _ =>
          val opIdx = Seq(eq.lastIndexOf('+'), eq.lastIndexOf('*')).max
          eq(opIdx) match {
            case '+' => basicNoParen(eq.take(opIdx)) + eq.slice(opIdx + 1, eq.length).toLong
            case '*' => basicNoParen(eq.take(opIdx)) * eq.slice(opIdx + 1, eq.length).toLong
          }
      }
    }

    def advancedNoParen(eq: String): Long = {
      eq.split('*').map(allPlus => allPlus.split('+').map(_.toLong).sum).product
    }
    println(input.map(str => eval(str)(basicNoParen)).sum)
    println(input.map(str => eval(str)(advancedNoParen)).sum)
  }
}
