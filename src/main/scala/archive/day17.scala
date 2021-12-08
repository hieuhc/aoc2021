package archive

import scala.io.Source
case class Point(x: Int, y: Int, z: Int, w: Int)
object day17 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromResource("input_day17.txt").getLines().toList
    val initGrid: Map[Point, Boolean] =
      (for (x <- 0 until input.head.length; y <- input.indices) yield (Point(x, y, 0, 0), input(y)(x) == '#')).toMap
    def updateGrid(minPoint: Point, maxPoint: Point, grid: Map[Point, Boolean]): (Point, Point, Map[Point, Boolean]) = {
      (
        Point(minPoint.x - 1, minPoint.y - 1, minPoint.z - 1, minPoint.w - 1),
        Point(maxPoint.x + 1, maxPoint.y + 1, maxPoint.z + 1, maxPoint.w + 1),
        (for (x <- minPoint.x - 1 to maxPoint.x + 1;
          y     <- minPoint.y - 1 to maxPoint.x + 1;
          z     <- minPoint.z - 1 to maxPoint.z + 1;
          w     <- minPoint.w - 1 to maxPoint.w + 1) yield {
          val neighborValues = (for (xOffset <- -1 to 1;
            yOffset <- -1 to 1;
            zOffset <- -1 to 1;
            wOffset <- -1 to 1) yield (xOffset, yOffset, zOffset, wOffset)).filter(_ != (0, 0, 0, 0)).map {
            case (xO, yO, zO, w0) =>
              val neighbor = Point(x + xO, y + yO, z + zO, w + w0)
              grid.getOrElse(neighbor, false)
          }
          val crrVal = grid.getOrElse(Point(x, y, z, w), false)
          val updatedVal =
            if (crrVal && List(2, 3).contains(neighborValues.count(_ == true)))
              true
            else if (!crrVal && neighborValues.count(_ == true) == 3)
              true
            else
              false
          (Point(x, y, z, w), updatedVal)
        }).toMap)
    }
    println((1 to 6).foldLeft((Point(0, 0, 0, 0), Point(input.head.length - 1, input.length - 1, 0, 0), initGrid)) {
      case ((minP, maxP, crrGird), _) =>
        val (updatedMinP, updatedMaxP, updatedGrid) = updateGrid(minP, maxP, crrGird)
        (updatedMinP, updatedMaxP, crrGird ++ updatedGrid)
    }._3.count(_._2 == true))
  }
}
