import scala.annotation.tailrec
import scala.io.Source
object day20 {
  def main(array: Array[String]): Unit = {
    type Pos = (Int, Int)
    type Mat = Map[Pos, Char]
    def posMove(p: Pos, d: Pos): Pos = (p._1 + d._1, p._2 + d._2)
    val input                        = Source.fromResource("input_day20.txt").getLines().toList
    def binToInt(str: String): Int = {
      str.indices.foldLeft(0) { case (num, idx) =>
        num + str(idx).asDigit * math.pow(2, str.length - 1 - idx).toInt
      }
    }
    val algorithm: String = input.head.map { c =>
      c match {
        case '.' => '0'
        case '#' => '1'
      }
    }
    val imageInput = input.drop(2)
    val inputMat: Mat = imageInput.indices.flatMap { y =>
      imageInput.head.indices.map { x =>
        val matV = imageInput(y)(x) match {
          case '.' => '0'
          case '#' => '1'
        }
        ((x, y), matV)
      }
    }.toMap

    def printMatrix(mat: Mat, topLeft: Pos, botRight: Pos): Unit = {
      val (xmin, ymin) = topLeft
      val (xmax, ymax) = botRight
      (ymin to ymax).foreach { y =>
        println((xmin to xmax).map(x =>
          mat.getOrElse((x, y), '0') match {
            case '0' => '.'
            case '1' => '#'
          }).mkString(""))
      }
    }

    @tailrec
    def transform(mat: Mat, count: Int, topLeft: Pos, botRight: Pos)(enhanceNum: Int): Mat = {
      if (count == enhanceNum)
        mat
      else {
        val newMat: Mat = (for {
          x <- topLeft._1 to botRight._1
          y <- topLeft._2 to botRight._2
        } yield (x, y)).map { case (x, y) =>
          val binIdx: String =
            List((-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)).map(d =>
              posMove(d, (x, y))).map(p => mat.getOrElse(p, if (count % 2 == 0) '0' else '1')).mkString("")
          val newV = algorithm(binToInt(binIdx))
          ((x, y), newV)
        }.toMap
        transform(newMat, count + 1, posMove(topLeft, (-1, -1)), posMove(botRight, (1, 1)))(enhanceNum)
      }
    }
    def resMat(enhanceNum: Int): Mat =
      transform(inputMat, 0, (-1, -1), (imageInput.head.length + 1, imageInput.length + 1))(enhanceNum)
    println(resMat(2).values.count(_ == '1'))
    println(resMat(50).values.count(_ == '1'))

  }

}
