package archive

import scala.io.Source
import scala.reflect.ClassTag

object day4 {
  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("input_day4.txt").getLines.toList
    val indexEmpty = input.indices.filter(input(_) == "")
    val items: List[List[String]] = (List(-1) ++ indexEmpty).zip(indexEmpty ++ List(input.length)).map {
      case (start, end) => input.slice(start + 1, end)
    }
    println(countValid[String](_.head)(checkValidPresent))
    println(countValid[(String, String)] { lst => (lst.head, lst(1)) }(checkValidFields))

    def countValid[T: ClassTag](extract: List[String] => T)(checkValid: List[T] => Boolean) = {
      items.map(item => item.flatMap(line => line.split(" ").map(field => extract(field.split(":").toList))))
        .count(checkValid)
    }
  }

  def checkValidPresent(fields: List[String]): Boolean = {
    List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").forall(fields.contains(_))
  }
  def isvalidNum(str: String, least: Int, most: Int): Boolean = {
    str.forall(_.isDigit) && str.toInt >= least && str.toInt <= most
  }
  def checkValidFields(fieldVals: List[(String, String)]): Boolean = {
    checkValidPresent(fieldVals.map(_._1)) && fieldVals.forall { case (name, value) =>
      name match {
        case "byr" => isvalidNum(value, 1920, 2002)
        case "iyr" => isvalidNum(value, 2010, 2020)
        case "eyr" => isvalidNum(value, 2020, 2030)
        case "hgt" => value match {
            case s"${num}cm" => isvalidNum(num, 150, 193)
            case s"${num}in" => isvalidNum(num, 59, 76)
            case _           => false
          }
        case "hcl" => """#([0-9a-f]){6}""".r matches value
        case "ecl" => List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
        case "pid" => value.forall(_.isDigit) && value.length == 9
        case "cid" => true
      }
    }
  }

}
