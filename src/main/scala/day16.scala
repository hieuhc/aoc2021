import scala.annotation.tailrec
import scala.io.Source

object day16 {
  def main(array: Array[String]): Unit = {
    val input: String = Source.fromResource("input_day16.txt").getLines().toList.head
    def hexToBin(str: String): String = {
      val hexMap: Map[Char, String] = Map(
        '0' -> "0000",
        '1' -> "0001",
        '2' -> "0010",
        '3' -> "0011",
        '4' -> "0100",
        '5' -> "0101",
        '6' -> "0110",
        '7' -> "0111",
        '8' -> "1000",
        '9' -> "1001",
        'A' -> "1010",
        'B' -> "1011",
        'C' -> "1100",
        'D' -> "1101",
        'E' -> "1110",
        'F' -> "1111"
      )
      str.map(c => hexMap(c)).mkString("")
    }
    def binToInt(str: String): BigInt = {
      str.indices.foldLeft(BigInt(0)) { case (num, idx) =>
        num + BigInt(str(idx).asDigit) * BigDecimal.valueOf(math.pow(2, str.length - 1 - idx)).toBigInt
      }
    }

    val binIn: String = hexToBin(input)
    println("binIn", binIn, "with length", binIn.length)

    @tailrec
    def literalParse(start: Int, crrV: String): (Int, String) = {
      val newV = crrV + binIn.slice(start + 1, start + 5)
      binIn(start).asDigit match {
        case 0 => (start + 4, newV)
        case 1 => literalParse(start + 5, newV)
      }
    }
    var versionSum: BigInt = 0L
    def singlePacketParse(start: Int): (Int, BigInt) = {
      val version: BigInt = binToInt(binIn.slice(start, start + 3))
      versionSum += version
      val typeId: Int = binToInt(binIn.slice(start + 3, start + 6)).toInt
      def operate(crrV: BigInt, f: (BigInt, BigInt) => BigInt): (Int, BigInt) = {
        binIn(start + 6).asDigit match {
          case 0 =>
            val totalLength = binToInt(binIn.slice(start + 7, start + 22)).toInt
            totalLengthPacketsParse(start + 22, totalLength)(crrV, f)
          case 1 =>
            val subPacketCount = binToInt(binIn.slice(start + 7, start + 18)).toInt
            countPacketsParse(start + 18, subPacketCount)(crrV, f)
        }
      }
      typeId match {
        case 4 =>
          val (end, value) = literalParse(start + 6, "")
          (end, binToInt(value))
        case 0 => operate(0, _ + _)
        case 1 => operate(1, _ * _)
        case 2 => operate(-1, { case (a, b) => if (a == -1) b else List(a, b).min })
        case 3 => operate(-1, { case (a, b) => if (a == -1) b else List(a, b).max })
        case 5 => operate(-1, { case (a, b) => if (a == -1) b else if (a > b) 1 else 0 })
        case 6 => operate(-1, { case (a, b) => if (a == -1) b else if (a < b) 1 else 0 })
        case 7 => operate(
            -1,
            { case (a, b) =>
              if (a == -1) b else if (a == b) 1 else 0
            })

      }
    }
    def totalLengthPacketsParse(start: Int, totalLength: Int)(
        crrV: BigInt,
        f: (BigInt, BigInt) => BigInt): (Int, BigInt) = {
      totalLength match {
        case 0 => (start - 1, crrV)
        case _ =>
          val (nextEnd, nextV) = singlePacketParse(start)
          val updatedV         = f(crrV, nextV)
          totalLengthPacketsParse(nextEnd + 1, totalLength - (nextEnd - start + 1))(updatedV, f)
      }
    }
    def countPacketsParse(start: Int, count: Int)(crrV: BigInt, f: (BigInt, BigInt) => BigInt): (Int, BigInt) = {
      count match {
        case 0 => (start - 1, crrV)
        case _ =>
          val (nextEnd, nextV) = singlePacketParse(start)
          val updatedV         = f(crrV, nextV)
          countPacketsParse(nextEnd + 1, count - 1)(updatedV, f)
      }
    }
    val (_, value) = singlePacketParse(0)
    println("versionSUm", versionSum)
    println("value", value)

  }

}
