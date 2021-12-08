package archive

import scala.io.Source

object day21 {
  def main(args: Array[String]): Unit = {
    val input: Seq[(Seq[String], Seq[String])] = Source.fromResource("input_day21.txt").getLines().toList.map {
      case s"${ingreStr} (contains ${aller})" => (ingreStr.split(" ").toSeq, aller.split(", ").toSeq)
    }

    val allergenMap: Map[String, Set[String]] = input.foldLeft(Map[String, Set[String]]()) {
      case (crrAllerMap, (ingredients, aller)) =>
        val newAllerMap: Map[String, Set[String]] = aller.map((_, ingredients.toSet)).toMap
        crrAllerMap ++ newAllerMap.map { case (k, v) =>
          (k, if (crrAllerMap.contains(k)) v.intersect(crrAllerMap(k)) else v)
        }
    }
    val ingreSet: Set[String] = input.flatMap(_._1).toSet
    val allerSet: Set[String] = input.flatMap(_._2).toSet
    def solve(alMap: Map[String, Set[String]]): Map[String, String] = {
      val ingredMap: Map[String, String] = alMap.filter { case (k, v) => v.size == 1 }.map { case (k, v) =>
        (v.head, k)
      }
      if (ingredMap.size == allerSet.size) ingredMap
      else {
        val updatedAlMap: Map[String, Set[String]] = alMap.map {
          case (k, v) if v.size > 1 =>
            (k, v.filterNot(ingredMap.contains))
          case x => x
        }
        solve(updatedAlMap)
      }
    }

    val nonSafeIngredientMap: Map[String, String] = solve(allergenMap)
    val nonAllerIngre                             = ingreSet.diff(nonSafeIngredientMap.keySet)
    println(input.map { case (ingre, _) => ingre.count(nonAllerIngre.contains) }.sum)
    println(nonSafeIngredientMap.toSeq.sortBy(_._2).map(_._1).mkString(","))
  }
}
