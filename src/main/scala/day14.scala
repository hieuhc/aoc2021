import scala.io.Source

object day14 {
  def main(args: Array[String]): Unit = {
    val input    = Source.fromResource("input_day14.txt").getLines().toList
    val template = input.head
    val stepMap: Map[String, String] = input.drop(2).map {
      case s"${k} -> ${v}" => (k, v)
    }.toMap

    def insert(pairMap: Map[String, Long]): Map[String, Long] = {
      val newPair: List[(String, Long)] =
        pairMap.filter(c => stepMap.contains(c._1)).toList.flatMap { case (str, count) =>
          List((s"${str.head}${stepMap(str)}", count), (s"${stepMap(str)}${str.last}", count))
        }
      val newPairCount: Map[String, Long] = newPair.groupBy(_._1).map { case (k, lst) => (k, lst.map(_._2).sum) }
      val noInsertedPairCount             = pairMap.filter(c => !stepMap.contains(c._1))
      newPairCount ++ noInsertedPairCount
    }
    val templateDoubleChaMap: Map[String, Long] = (template.dropRight(1) zip template.drop(1)).map { case (c1, c2) =>
      (s"$c1$c2", 1L)
    }.toMap
    def countChar(steps: Int) = {
      val pairCount = (0 until steps).foldLeft(templateDoubleChaMap) { case (crrM, _) => insert(crrM) }
      val charCount: Map[Char, Long] = pairCount.toList.flatMap { case (double, count) =>
        List((double.head, count), (double.last, count))
      }.groupBy(_._1).map { case (k, lst) => (k, lst.map(_._2).sum) }
      val finalCharCount = charCount ++
        Map(template.head -> (charCount(template.head) + 1L), template.last -> (charCount(template.last) + 1L))

      (finalCharCount.values.max - finalCharCount.values.min) / 2
    }
    println(countChar(10))
    println(countChar(40))
  }

}
