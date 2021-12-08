package archive

import scala.io.Source

object day7 {
  type Bag = (Int, String)
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input_day7.txt").getLines().toList
    def extractGraph(mapFunc: (String, Set[Bag]) => Map[String, Set[Bag]]): Map[String, Set[Bag]] =
      input.foldLeft(Map[String, Set[Bag]]()) { case (crrGraph, line) =>
        val (bagContainer, bagContainedList) = line match {
          case s"${container} bags contain ${containedListStr}." =>
            (
              container,
              containedListStr.split(", ").collect {
                case s"${num} ${contained} bag${_}" if contained != "other" => (num.toInt, contained)
              })
        }
        val lineMap = mapFunc(bagContainer, bagContainedList.toSet)
        crrGraph ++ lineMap.map { case (k, v) => k -> (v ++ crrGraph.getOrElse(k, Set[Bag]())) }
      }

    val containedGraph = extractGraph((a, b) => b.map(tup => (tup._2, Set((0, a)))).toMap)
    def countContainer(bag: String, visited: Set[String]): (Int, Set[String]) = {
      if (!visited.contains(bag)) {
        containedGraph.getOrElse(bag, Set()).foldLeft((1, visited ++ Set(bag))) {
          case ((crrCount: Int, crrVisited: Set[String]), (_, parent)) =>
            val (newCount, newVisited) = countContainer(parent, crrVisited)
            (crrCount + newCount, crrVisited ++ newVisited)
        }
      } else
        (0, visited)
    }
    println(countContainer("shiny gold", Set())._1 - 1)

    val containerGraph: Map[String, Set[Bag]] = extractGraph((a, b) => Map(a -> b))
    def countBag(bag: String, cachedContainer: Map[String, Int]): (Int, Map[String, Int]) = {
      if (cachedContainer.contains(bag)) (cachedContainer(bag), cachedContainer)
      else {
        val countChild =
          containerGraph.getOrElse(bag, Set[Bag]()).foldLeft((0, cachedContainer)) {
            case ((crrCount: Int, crrCached: Map[String, Int]), (childNum: Int, childBag: String)) =>
              val (bagChildCount, newCached) = countBag(childBag, crrCached)
              (crrCount + childNum * bagChildCount, crrCached ++ newCached)
          }
        (countChild._1 + 1, countChild._2 ++ Map((bag, countChild._1 + 1)))
      }
    }
    println(countBag("shiny gold", Map())._1 - 1)

  }

}
