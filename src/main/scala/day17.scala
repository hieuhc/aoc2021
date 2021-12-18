object day17 {
  def main(args: Array[String]): Unit = {
    val (xMin, xMax) = (185, 221)
    val (yMin, yMax) = (-122, -74)

    val veloXMax = xMax
    val veloXMin = (1 to xMin).find(num => num * (num + 1) / 2 > xMin).get

    val vxValidSteps: List[(Int, List[Int])] = (veloXMin to veloXMax).map { vx =>
      val validSteps = (1 to vx).filter { step =>
        val x: Double = step * (2 * vx - step + 1) / 2.0
        (xMin <= x) && (x <= xMax)
      }
      (vx, validSteps.toList)
    }.toList
    def findHighestVeloY(step: Int): Option[Int] = {
      val lowerBound: Double = yMin / step.toDouble + (step - 1) / 2.0
      val upperBound: Double = yMax / step.toDouble + (step - 1) / 2.0
      val intLowerBound: Int = if (lowerBound.toInt == lowerBound) lowerBound.toInt else lowerBound.toInt + 1
      (intLowerBound to upperBound.toInt).toList match {
        case Nil => None
        case lst => Some(lst.max)
      }
    }
    val isNoBoundForXStep = vxValidSteps.exists { case (vx, validSteps) => vx == validSteps.max }
    val maxSteps          = if (isNoBoundForXStep) 999999999 else vxValidSteps.map(_._1).max
    val highestVeloY: Int = (1 until maxSteps).flatMap { step =>
      findHighestVeloY(step)
    }.max
    println(highestVeloY * (highestVeloY + 1) / 2)

//    part 2
    val (veloYMin, veloYMax) = (yMin, highestVeloY)
    def findStepForB(b: Int, crrK: Int, kSet: Set[Int]): Set[Int] = {
      val y: Double = crrK * (2 * b - crrK + 1) / 2.0
      if (y < yMin) kSet
      else {
        val newSet = if (y <= yMax) kSet + crrK else kSet
        findStepForB(b, crrK + 1, newSet)
      }
    }
    val countValidByB = (veloYMin to veloYMax).map { b =>
      val eligibleK = findStepForB(b, 1, Set())
      eligibleK.flatMap { k =>
        (veloXMin to veloXMax).filter { a =>
          val kTrim     = if (k > a) a else k
          val x: Double = kTrim * (2 * a - kTrim + 1) / 2.0
          (xMin <= x) && (x <= xMax)
        }
      }.size
    }
    println(countValidByB.sum)

  }
}
