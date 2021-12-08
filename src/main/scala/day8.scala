import scala.io.Source
object day8 {
  def main(array: Array[String]): Unit = {
    val input                         = Source.fromResource("input_day8.txt").getLines().toList
    val outputVal: List[List[String]] = input.map(_.split(" \\| ").last.split(" ").toList)

    //    part 1
    println(outputVal.map(lst => lst.count(e => List(2, 4, 3, 7).contains(e.length))).sum)

    //    part 2

    // decode the input part by counting the frequency of signals.
    def decode(strList: List[String]): Map[Char, Char] = {
      val charCount: Map[Char, Int] = strList.foldLeft(Map[Char, Int]()) { case (crrCount, str) =>
        val strToMap: Map[Char, Int] = str.map(c => (c, 1)).toMap
        crrCount ++ strToMap.map { case (ch, co) => (ch, co + crrCount.getOrElse(ch, 0)) }
      }
      val countCharMap = charCount.map(_.swap)
      val initRes      = Map(countCharMap(4) -> 'e', countCharMap(6) -> 'b', countCharMap(9) -> 'f')
      val four         = strList.find(_.length == 4).get

      val countSeven = charCount.filter(_._2 == 7).keys      // d or g
      val d          = countSeven.find(four.contains(_)).get // 4 has d but not g
      val g          = countSeven.find(_ != d).get

      val countEight = charCount.filter(_._2 == 8).keys      // a or c
      val c          = countEight.find(four.contains(_)).get // 4 has c but not a
      val a          = countEight.find(_ != c).get
      initRes ++ Map(a -> 'a', c -> 'c', d -> 'd', g -> 'g')
    }
    def formDigit(charMap: Map[Char, Char], strList: List[String]): Long = {
      val signalMap: Map[String, Int] = Map(
        "abcefg"  -> 0,
        "cf"      -> 1,
        "acdeg"   -> 2,
        "acdfg"   -> 3,
        "bcdf"    -> 4,
        "abdfg"   -> 5,
        "abdefg"  -> 6,
        "acf"     -> 7,
        "abcdefg" -> 8,
        "abcdfg"  -> 9
      )
      val realStrList = strList.map(str => str.map(c => charMap(c)).sorted)
      realStrList.map(signalMap(_)).map(_.toString).mkString("").toLong
    }
    val inputVal: List[List[String]] = input.map(_.split(" \\| ").head.split(" ").toList)
    val tmp: List[Long] = inputVal.zip(outputVal).map { case (inLst, outLst) =>
      val chMap = decode(inLst)
      formDigit(chMap, outLst)
    }
    println(tmp.sum)
  }

}
