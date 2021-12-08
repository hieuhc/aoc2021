package archive

import scala.io.Source

object day14 {
  object long {
    def unapply(arg: String): Option[Long] = arg.toLongOption
  }
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day14.txt").getLines().toList
    def binaryToLong(s: Seq[Char]): Long = s.foldRight((0L, 0)) { case (ch: Char, (res: Long, ind: Int)) =>
      (res + (ch.asDigit * math.pow(2, ind)).toLong, ind + 1)
    }._1

    def maskValue(mask: String, binary: String): Long = {
      binaryToLong((List.fill(mask.length - binary.length)('0').mkString("") + binary).zip(mask).map {
        case (ch: Char, ma: Char) =>
          ma match {
            case 'X' => ch
            case _   => ma
          }
      })
    }
    val sol1Mem =
      input.foldLeft((Map[Long, Long](), "")) { case ((memory: Map[Long, Long], crrMask: String), in: String) =>
        in match {
          case s"mask = ${newMask}" => (memory, newMask)
          case s"mem[${long(address)}] = ${long(value)}" =>
            (memory + (address -> maskValue(crrMask, value.toBinaryString)), crrMask)
        }
      }._1
    println(sol1Mem.values.sum)

    def maskAddress(mask: String, addressBinary: String): Seq[Long] = {
      (List.fill(mask.length - addressBinary.length)('0').mkString("") + addressBinary).zip(mask).map {
        case (ch: Char, ma: Char) =>
          ma match {
            case '0' => ch
            case _   => ma
          }
      }.foldLeft(List[String]("")) { case (gen: List[String], ch: Char) =>
        ch match {
          case '0' | '1' => gen.map(str => str + ch)
          case _         => gen.map(str => str + '0') ++ gen.map(str => str + '1')
        }
      }.map(binaryToLong(_))
    }
    val sol2Mem: Map[Long, Long] = {
      input.foldLeft((Map[Long, Long](), "")) { case ((memory: Map[Long, Long], crrMask: String), in: String) =>
        in match {
          case s"mask = ${newMask}" => (memory, newMask)
          case s"mem[${long(address)}] = ${long(value)}" =>
            val updatedMem: Map[Long, Long] = {
              maskAddress(crrMask, address.toBinaryString).map(newAd => (newAd, value)).toMap
            }
            (memory ++ updatedMem, crrMask)
        }
      }._1
    }
    println(sol2Mem.values.sum)
  }

}
