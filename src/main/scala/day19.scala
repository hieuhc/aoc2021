import scala.annotation.tailrec
import scala.io.Source
object day19 {
  object int {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
  def main(array: Array[String]): Unit = {
    type Pos       = (Int, Int, Int)
    type Dist      = (Int, Int, Int)
    type Scanner   = List[Pos]
    type Transform = Pos => Pos

    def PosDist(p1: Pos, p2: Pos): Dist = (p2._1 - p1._1, p2._2 - p1._2, p2._3 - p1._3)
    def PosMove(p1: Pos, d: Dist): Pos  = (p1._1 + d._1, p1._2 + d._2, p1._3 + d._3)

    val input = Source.fromResource("input_day19.txt").getLines().toList
//    val input = Source.fromResource("example.txt").getLines().toList
    val scannerList: List[Scanner] = input.foldLeft(List.empty[Scanner]) {
      case (crrSL, line) => line match {
          case s"${int(p1)},${int(p2)},${int(p3)}" => crrSL.dropRight(1) :+ (crrSL.last :+ (p1, p2, p3))
          case ""                                  => crrSL
          case _                                   => crrSL :+ Nil
        }
    }

    def transformPos(p: Pos): List[Pos] = {
      val (x, y, z) = p
      List(
        p,
        (x, -z, y),
        (x, -y, -z),
        (x, z, -y),
        (-x, -y, z),
        (-x, -z, -y),
        (-x, y, -z),
        (-x, z, y),
        (-z, x, -y),
        (y, x, -z),
        (z, x, y),
        (-y, x, z),
        (z, -x, -y),
        (y, -x, z),
        (-z, -x, y),
        (-y, -x, -z),
        (-y, -z, x),
        (z, -y, x),
        (y, z, x),
        (-z, y, x),
        (z, y, -x),
        (-y, z, -x),
        (-z, -y, -x),
        (y, -z, -x)
      )
    }
    val transformFuncList: List[Transform] = {
      (0 until 24).map(i => (pos: Pos) => transformPos(pos)(i))
    }.toList

    def overlappedScanner(s1: Scanner, s2: Scanner): Option[Transform] = {
      val s1DistCross: List[List[Dist]] = s1.map(p1 => s1.map(p2 => PosDist(p1, p2)))
      transformFuncList.foldLeft(Option.empty[Transform]) { case (foundTransform: Option[Transform], func: Transform) =>
        foundTransform match {
          case Some(transformFunc) => Some(transformFunc)
          case None =>
            val s2Tran: Scanner               = s2.map(func)
            val s2DistCross: List[List[Dist]] = s2Tran.map(p1 => s2Tran.map(p2 => PosDist(p1, p2)))
            (for {
              s1Idx <- s1DistCross.indices
              s2Idx <- s2DistCross.indices
            } yield (s1Idx, s2Idx)).find { case (i1, i2) =>
              s1DistCross(i1).intersect(s2DistCross(i2)).length > 11
            } match {
              case Some((i1, i2)) => Some((p: Pos) => PosMove(func(p), PosDist(s2Tran(i2), s1(i1))))
              case None           => None
            }
        }

      }

    }
    val overlappedMap: Map[Int, Map[Int, Transform]] = {
      (for {
        i1 <- scannerList.indices
        i2 <- scannerList.indices
      } yield (i1, i2)).flatMap { case (i1, i2) =>
        overlappedScanner(scannerList(i1), scannerList(i2)) match {
          case Some(t) => List((i2, (i1, t)))
          case None    => Nil
        }
      }.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).toMap) }
    }

    @tailrec
    def resolvedOverlappedMap(toS0Map: Map[Int, Transform]): Map[Int, Transform] = {
      if (toS0Map.size == scannerList.length) toS0Map
      else {
        val updatedToS0Map: Map[Int, Transform] = overlappedMap.filterNot(m => toS0Map.contains(m._1)).flatMap {
          case (scannerIdx, tranMap) =>
            if (tranMap.contains(0)) List((scannerIdx, tranMap(0)))
            else {
              tranMap.keys.find(toS0Map.contains) match {
                case None => Nil
                case Some(middleIdx) =>
                  List((scannerIdx, (p: Pos) => toS0Map(middleIdx)(tranMap(middleIdx)(p))))
              }

            }
        }
        val newS0 = toS0Map ++ updatedToS0Map
        resolvedOverlappedMap(newS0)
      }
    }
    val toS0MapFinal = resolvedOverlappedMap(Map[Int, Transform]())
    println("toS0MapFinal", toS0MapFinal)
    val scannerListToS0: List[Scanner] = scannerList.indices.map {
      case 0 => scannerList.head
      case n => scannerList(n).map(toS0MapFinal(n))
    }.toList
    val countPoints: Int = scannerListToS0.reduce[Scanner](_ ++ _).toSet.size
    println(countPoints)

    val scannerPosRelWithS0: List[Pos] = toS0MapFinal.values.map(f => f((0, 0, 0))).toList
    println(scannerPosRelWithS0.combinations(2).map {
      case s1 :: s2 :: Nil => math.abs(s1._1 - s2._1) + math.abs(s1._2 - s2._2) + math.abs(s1._3 - s2._3)
    }.max)

  }

}
