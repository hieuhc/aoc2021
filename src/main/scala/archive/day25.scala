package archive

import scala.io.Source

object day25 {
  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("input_day25.txt").getLines().toList
    val cardPK: Long = input.head.toLong
    val doorPK: Long = input.drop(1).head.toLong

    def findLoopSize(subjectNumber: Long, publicKey: Long, crrVal: Long, crrLoop: Long): Long = {
      if (crrVal == publicKey) crrLoop
      else {
        val newVal = (crrVal * subjectNumber) % 20201227L
        findLoopSize(subjectNumber, publicKey, newVal, crrLoop + 1)
      }
    }
    val cardLoopSize = findLoopSize(7L, cardPK, 1L, 0L)
    def encrypt(loopSize: Long, subjectNumber: Long): Long = {
      (0 until loopSize.toInt).foldLeft(1L) { case (crrEncryptVal, _) => (crrEncryptVal * subjectNumber) % 20201227L }
    }
    val encryptedByCard = encrypt(cardLoopSize, doorPK)
    println(encryptedByCard)
  }
}
