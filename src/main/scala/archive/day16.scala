package archive

import scala.io.Source

object day16 {
  object int {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
  def main(args: Array[String]): Unit = {
    val input    = Source.fromResource("input_day16.txt").getLines().toList
    val splitIdx = input.indices.filter(input(_) == "")
    val fieldRange: Map[String, ((Int, Int), (Int, Int))] =
      input.slice(0, splitIdx.head).map { case s"${field}: ${int(b1)}-${int(e1)} or ${int(b2)}-${int(e2)}" =>
        (field, ((b1, e1), (b2, e2)))
      }.toMap
    val allRange: Seq[(Int, Int)] = fieldRange.values.flatMap(tup => Seq(tup._1, tup._2)).toSeq
    val nearByTickets: Seq[Seq[Int]] =
      input.slice(splitIdx(1) + 2, input.length).map(str => str.split(",").map(_.toInt))
    val invalidNums: Seq[Int] =
      nearByTickets.flatten.filterNot(t => allRange.exists { case (b, e) => b <= t && t <= e })
    println(invalidNums.sum)

//    part 2
    val validTickets: Seq[Seq[Int]] = nearByTickets.filterNot(ticket => ticket.exists(invalidNums.contains(_)))
    val ticket: Seq[Int]            = input(splitIdx.head + 2).split(",").map(_.toInt)
    val indToFieldNames: Map[Int, Set[String]] = ticket.indices.map { case ind: Int =>
      val allValAtInd: Seq[Int] = validTickets.map(_(ind))
      val fieldName: Set[String] = allValAtInd.foldLeft(fieldRange.keySet) { case (crrSet: Set[String], v: Int) =>
        val belongSet: Set[String] = fieldRange.filter { case (_, ((b1, e1), (b2, e2))) =>
          (b1 <= v && v <= e1) || (b2 <= v && v <= e2)
        }.keySet
        val res = crrSet.intersect(belongSet)
        res
      }
      (ind, fieldName)
    }.toMap
    val resolveMap: Map[Int, String] =
      indToFieldNames.toList.sortBy(tup => tup._2.size).foldLeft((Set[String](), Map[Int, String]())) {
        case ((crrSet, crrMap), (ind, fieldSet)) =>
          val left = fieldSet.diff(crrSet).head
          (crrSet + left, crrMap + (ind -> left))
      }._2
    println(ticket.indices.filter(ind => resolveMap(ind).contains("departure")).map(ticket(_).toLong).product)

  }

}
