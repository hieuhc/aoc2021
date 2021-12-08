package archive

object day23 {
  def main(args: Array[String]): Unit = {
    val input: String = "318946572"
    def findDest(crr: Long, except: Seq[Long], maxNum: Long): Long = {
      (crr - 1 until 0 by -1).find(v => !except.contains(v)) match {
        case None    => (maxNum until 0 by -1).find(v => !(except :+ crr).contains(v)).get
        case Some(v) => v
      }
    }
    def playRound(
        crrNum: Long,
        crrInput: Map[Long, Long],
        maxNum: Long,
        numRound: Int,
        target: Int): Map[Long, Long] = {
      if (numRound == target) crrInput
      else {
        val nextThree = Seq(crrInput(crrNum), crrInput(crrInput(crrNum)), crrInput(crrInput(crrInput(crrNum))))
        val destNum   = findDest(crrNum, nextThree, maxNum)
        val updatedIn = crrInput ++ Map(crrNum -> crrInput(nextThree.last)) ++
          Map(nextThree.last -> crrInput(destNum)) ++ Map(destNum -> nextThree.head)

        playRound(updatedIn(crrNum), updatedIn, maxNum, numRound + 1, target)
      }
    }
    def makeLinkedList(in: Seq[Long]): Map[Long, Long] = (in zip (in.tail :+ in.head)).toMap
    val inputLst: Seq[Long]                            = input.map(_.asDigit.toLong)
    val part1In: Map[Long, Long]                       = makeLinkedList(inputLst)
    val resPart1                                       = playRound(inputLst.head, part1In, inputLst.max, 0, 100)
    def access(res: String, crr: Long): String         = if (crr == 1) res else access(res + crr.toString, resPart1(crr))
    println(access("", resPart1(1)))

    val part2In  = makeLinkedList(inputLst ++ (10L until 1000001L))
    val resPart2 = playRound(inputLst.head, part2In, 1000000L, 0, 10000000)
    println(resPart2(1) * resPart2(resPart2(1)))
  }

}
