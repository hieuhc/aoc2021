import scala.io.Source
object day22 {
  object int {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
  type Segment = (Int, Int)
  type Point   = (Int, Int, Int)
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day22.txt").getLines().toList

    def segmentOverlap(seg1: Segment, seg2: Segment): Option[Segment] = {
      if (seg1 == seg2) return Some(seg1)
      val first    = if (seg1._1 < seg2._1) seg1 else seg2
      val second   = Set(seg1, seg2).diff(Set(first)).head
      val (_, y1)  = first
      val (x2, y2) = second
      if (y1 < x2) None
      else if (x2 <= y1 && y1 <= y2) Some((x2, y1))
      else Some((x2, y2))
    }

    def segmentToList(s: Option[Segment]): List[Int] = s.flatMap { case (min, max) =>
      Some((min to max).toList)
    }.getOrElse(Nil)

    val cubesOnInRange: Set[Point] = input.foldLeft(Set[Point]()) { case (crrSet, line) =>
      line match {
        case s"${switch} x=${int(xMin)}..${int(xMax)},y=${int(yMin)}..${int(yMax)},z=${int(zMin)}..${int(zMax)}" =>
          val limit               = (-50, 50)
          val xOverlap: List[Int] = segmentToList(segmentOverlap((xMin, xMax), limit))
          val yOverlap: List[Int] = segmentToList(segmentOverlap((yMin, yMax), limit))
          val zOverlap: List[Int] = segmentToList(segmentOverlap((zMin, zMax), limit))
          val lineSet: Set[Point] = (for {
            x <- xOverlap
            y <- yOverlap
            z <- zOverlap
          } yield (x, y, z)).toSet
          switch match {
            case "on"  => crrSet.union(lineSet)
            case "off" => crrSet.diff(lineSet)
          }
      }
    }
    println(cubesOnInRange.size)

    def addSegment(segList: List[Segment], newSeg: Segment): List[Segment] = {
      val sortedSegList = (segList :+ newSeg).sortBy(_._1)
      val res = sortedSegList.foldLeft(List[Segment]()) { case (crrList, seg) =>
        crrList match {
          case Nil => List(seg)
          case lastSeg :: tail =>
            if (seg._1 > lastSeg._2) seg :: crrList
            else if (seg._1 <= lastSeg._2 && lastSeg._2 <= seg._2) (lastSeg._1, seg._2) :: tail
            else crrList
        }
      }
      res
    }

    def removeSegment(segList: List[Segment], removeSeg: Segment): List[Segment] = {
      val res = segList.foldLeft(List[Segment]()) { case (crrList, seg) =>
        val (x, y) = seg
        segmentOverlap(seg, removeSeg) match {
          case None => seg :: crrList
          case Some((oMin, oMax)) =>
            List((x, oMin - 1), (oMax + 1, y)).filter { case (a, b) => a <= b } ++ crrList
        }
      }
      res
    }
    def mergeSeg(segList: List[Segment], seg: Segment, operator: String): List[Segment] = {
      operator match {
        case "on"  => addSegment(segList, seg)
        case "off" => removeSegment(segList, seg)
      }
    }
    def merge2D(
        state: Map[Segment, List[Segment]],
        plane: (Segment, Segment),
        operator: String): Map[Segment, List[Segment]] = {
      val (ySeg, xSeg) = plane
      val (stateWithoutNewSeg, overlapLst) =
        state.keys.toList.sortBy(_._1).foldLeft((Map[Segment, List[Segment]](), List[Segment]())) {
          case ((crrState, crrSegList), seg) =>
            segmentOverlap(seg, ySeg) match {
              case None => (crrState ++ Map(seg -> state(seg)), crrSegList)
              case Some((oMin, oMax)) =>
                val (segX, segY) = seg
                val updatedState = List((segX, oMin - 1), (oMax + 1, segY)).filter { case (a, b) => a <= b }.map(s =>
                  (s, state(seg))).toMap ++ Map((oMin, oMax) -> mergeSeg(state(seg), xSeg, operator))
                (crrState ++ updatedState, crrSegList :+ (oMin, oMax))
            }
        }
      val sortedOverlapLst = overlapLst.sortBy(_._1)
      val newSegList: List[Segment] = {
        if (operator == "off") Nil
        else if (sortedOverlapLst.nonEmpty)
          (List((ySeg._1, sortedOverlapLst.head._1 - 1), (sortedOverlapLst.last._2 + 1, ySeg._2)) ++
            sortedOverlapLst.dropRight(1).indices.map(idx =>
              (sortedOverlapLst(idx)._2 + 1, sortedOverlapLst(idx + 1)._1 - 1))).filter { case (a, b) => a <= b }
        else List(ySeg)
      }
      val res = stateWithoutNewSeg ++ newSegList.map((_, List(xSeg)))
      res
    }
    def merge3D(
        state: Map[Segment, Map[Segment, List[Segment]]],
        cube: (Segment, Segment, Segment),
        operator: String): Map[Segment, Map[Segment, List[Segment]]] = {
      val (zSeg, ySeg, xSeg) = cube
      val (stateWithoutNewSeg, overlapLst) =
        state.keys.toList.sortBy(_._1).foldLeft((Map[Segment, Map[Segment, List[Segment]]](), List[Segment]())) {
          case ((crrState, crrSegList), seg) =>
            segmentOverlap(seg, zSeg) match {
              case None => (crrState ++ Map(seg -> state(seg)), crrSegList)
              case Some((oMin, oMax)) =>
                val (segX, segY) = seg
                val updatedState = List((segX, oMin - 1), (oMax + 1, segY)).filter { case (a, b) => a <= b }.map(s =>
                  (s, state(seg))).toMap ++ Map((oMin, oMax) -> merge2D(state(seg), (ySeg, xSeg), operator))
                (crrState ++ updatedState, crrSegList :+ (oMin, oMax))
            }
        }
      val sortedOverlapLst = overlapLst.sortBy(_._1)
      val newSegList: List[Segment] = {
        if (operator == "off") Nil
        else if (sortedOverlapLst.nonEmpty)
          (List((zSeg._1, sortedOverlapLst.head._1 - 1), (sortedOverlapLst.last._2 + 1, zSeg._2)) ++
            sortedOverlapLst.dropRight(1).indices.map(idx =>
              (sortedOverlapLst(idx)._2 + 1, sortedOverlapLst(idx + 1)._1 - 1))).filter { case (a, b) => a <= b }
        else List(zSeg)
      }
      val res = stateWithoutNewSeg ++ newSegList.map((_, Map(ySeg -> List(xSeg))))
      res
    }
    val part2: Map[Segment, Map[Segment, List[Segment]]] =
      input.foldLeft(Map[Segment, Map[Segment, List[Segment]]]()) { case (crrState, line) =>
        line match {
          case s"${switch} x=${int(xMin)}..${int(xMax)},y=${int(yMin)}..${int(yMax)},z=${int(zMin)}..${int(zMax)}" =>
            merge3D(crrState, ((zMin, zMax), (yMin, yMax), (xMin, xMax)), switch)
        }
      }
    def len(s: Segment): Long = s._2 - s._1 + 1
    def planeArea(m: Map[Segment, List[Segment]]): Long = m.toList.map { case (seg, lstSeg) =>
      len(seg) * lstSeg.map(len).sum
    }.sum
    val size: Long = part2.toList.map { case (zSeg, plane) => len(zSeg) * planeArea(plane) }.sum
    println(size)

  }

}
