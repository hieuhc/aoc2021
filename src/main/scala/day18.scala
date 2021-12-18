import scala.annotation.tailrec
import scala.io.Source
object day18 {
  type SnailFish = List[(Int, Int)]
  def main(args: Array[String]): Unit = {
    val snailFishList: List[String] = Source.fromResource("input_day18.txt").getLines().toList
    def parseFish(fish: String): SnailFish = {
      fish.foldLeft((0, false, List[(Int, Int)]())) { case ((crrDepth, isNumCont, crrLst), char) =>
        char match {
          case '[' => (crrDepth + 1, false, crrLst)
          case ']' => (crrDepth - 1, false, crrLst)
          case ',' => (crrDepth, false, crrLst)
          case num =>
            val updatedList =
              if (!isNumCont)(crrLst :+ (crrDepth, num.asDigit))
              else (crrLst.dropRight(1) :+ (crrDepth, crrLst.last._2 * 10 + num.asDigit))
            (crrDepth, true, updatedList)
        }
      }
    }._3
    def explode(fish: SnailFish): Option[SnailFish] = {
      fish.indices.find(idx => fish(idx)._1 == 5 && (idx + 1 < fish.length) && (fish(idx + 1)._1 == 5)) match {
        case None => None
        case Some(explodeIdx) => Some(fish.indices.foldLeft(List[(Int, Int)]()) { case (crrNewFish, idx) =>
            if (idx == explodeIdx - 1)
              crrNewFish :+ (fish(idx)._1, fish(idx)._2 + fish(explodeIdx)._2)
            else if (idx == explodeIdx) crrNewFish :+ (fish(explodeIdx)._1 - 1, 0)
            else if (idx == explodeIdx + 1) crrNewFish
            else if (idx == explodeIdx + 2)
              crrNewFish :+ (fish(idx)._1, fish(idx)._2 + fish(explodeIdx + 1)._2)
            else crrNewFish :+ fish(idx)
          })
      }
    }
    def split(fish: SnailFish): Option[SnailFish] = {
      fish.indices.find(idx => fish(idx)._2 >= 10) match {
        case None => None
        case Some(splitIdx) =>
          val newDepth = fish(splitIdx)._1 + 1
          val splitVal = fish(splitIdx)._2
          Some((fish.take(splitIdx) :+ (newDepth, splitVal / 2) :+
            (newDepth, if (splitVal / 2.0 == splitVal / 2) splitVal / 2 else splitVal / 2 + 1)) ++
            fish.takeRight(fish.length - splitIdx - 1))
      }
    }

    @tailrec
    def action(fish: SnailFish): SnailFish = {
      explode(fish) match {
        case None =>
          split(fish) match {
            case None            => fish
            case Some(splitFish) => action(splitFish)
          }
        case Some(explodedFish) => action(explodedFish)
      }
    }

    def addition(first: SnailFish, second: SnailFish): SnailFish = {
      (first ++ second).map { case (depth, v) => (depth + 1, v) }
    }
    @tailrec
    def magnitude(fish: SnailFish): Long = {
      if (fish.length == 1) fish.head._2
      else {
        val newFish = (1 until fish.length).foldLeft((true, List[(Int, Int)](fish.head))) {
          case ((expectedPair, crrFish), idx) =>
            val lastVal = crrFish.last._2
            val crrVal  = fish(idx)._2
            if (expectedPair && crrFish.last._1 == fish(idx)._1)
              (false, crrFish.dropRight(1) :+ (fish(idx)._1 - 1, lastVal * 3 + crrVal * 2))
            else (expectedPair, crrFish :+ fish(idx))
        }._2
        magnitude(newFish)
      }
    }
    val finalFish = snailFishList.map(parseFish).reduce[SnailFish] {
      case (first: SnailFish, second: SnailFish) =>
        action(addition(first, second))
    }
    println(magnitude(finalFish))

    println(snailFishList.map(parseFish).combinations(2).flatMap { twoFish =>
      val (f1, f2) = (twoFish.head, twoFish.last)
      List((f1, f2), (f2, f1)).map { case (x, y) => magnitude(action(addition(x, y))) }
    }.max)

  }

}
