package archive

import scala.io.Source

object day19 {
  object int {
    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("input_day19.txt").getLines().toList.filterNot(_ == "")
    val (initValueMap, initRuleMap, messages) =
      input.foldLeft((Map[Int, Seq[String]](), Map[Int, Seq[Seq[Int]]](), Seq[String]())) {
        case ((crrEvalMap, crrRuleMap, crrMessages), line) =>
          line match {
            case s"${int(num)}: ${value}" =>
              if (value.contains("\"")) {
                (crrEvalMap + (num -> Seq(value(1).toString)), crrRuleMap, crrMessages)
              } else {
                val ruleSeq: Seq[Seq[Int]] = value.split("\\|").map(rule => rule.trim.split(" ").map(_.toInt).toSeq)
                (crrEvalMap, crrRuleMap + (num -> ruleSeq), crrMessages)
              }
            case _ => (crrEvalMap, crrRuleMap, crrMessages :+ line)
          }
      }
    def evalRule(rule: Seq[Int], valueMap: Map[Int, Seq[String]])(ruleMap: Map[Int, Seq[Seq[Int]]]) = {
      val (updatedValMap, subRuleVal) =
        rule.foldLeft((valueMap, Seq[Seq[String]]())) { case ((crrValMap, crrValSeq: Seq[Seq[String]]), ruleNum) =>
          val (ruleVal, ruleValMap) = eval(ruleNum, crrValMap)(ruleMap)
          (crrValMap ++ ruleValMap, crrValSeq :+ ruleVal)
        }
      val nodeVal: Seq[String] = subRuleVal.foldLeft(Seq("")) { case (crrResVal: Seq[String], subVal: Seq[String]) =>
        for (crrStr <- crrResVal;
          subStr    <- subVal) yield (crrStr + subStr)
      }
      (nodeVal, updatedValMap)
    }
    def eval(
        node: Int,
        valueMap: Map[Int, Seq[String]])(ruleMap: Map[Int, Seq[Seq[Int]]]): (Seq[String], Map[Int, Seq[String]]) = {
      if (valueMap.contains(node)) (valueMap(node), valueMap)
      else {
        val (nodeVal, newValueMap) =
          ruleMap(node).foldLeft((Seq[String](), valueMap)) { case ((crrNodeVal, crrValueMap), rule) =>
            val (ruleVal, updatedValueMap) = evalRule(rule, crrValueMap)(ruleMap)
            (crrNodeVal ++ ruleVal, crrValueMap ++ updatedValueMap)
          }

        (nodeVal, newValueMap + (node -> nodeVal))
      }
    }
    val candidates = eval(0, initValueMap)(initRuleMap)
    println(messages.count(candidates._1.contains(_)))

//    solution 2
    val rule42: Seq[String] = candidates._2(42)
    val rule31: Seq[String] = candidates._2(31)
    def matchX(str: String): Boolean =
      if (str.isEmpty) true
      else
        rule42.filter(p => str.startsWith(p)).exists(p => matchX(str.drop(p.length)))

    def matchY(str: String): Boolean = {
      if (str.isEmpty) true
      else {
        val startWRule42 = rule42.filter(p => str.startsWith(p))
        val endWRule31   = rule31.filter(p => str.endsWith(p))
        (startWRule42 zip endWRule31).exists { case (p42, p31) =>
          if (str.length >= p42.length + p31.length)
            matchY(str.drop(p42.length).dropRight(p31.length))
          else false
        }
      }
    }

    def matchPattern(str: String, pattern: String): Boolean = {
      if (str.isEmpty && pattern.isEmpty) true
      else if (pattern.isEmpty) false
      else {
        pattern.head match {
          case 'a' | 'b' if pattern.head == str.head => matchPattern(str.tail, pattern.tail)
          case 'x' =>
            (1 to str.length).exists(idx => matchX(str.take(idx)) && matchPattern(str.drop(idx), pattern.tail))
          case 'y' =>
            (1 to str.length).exists(idx => matchY(str.take(idx)) && matchPattern(str.drop(idx), pattern.tail))
          case _ => false
        }
      }
    }
    val initMapValuesProb2: Map[Int, Seq[String]] = initValueMap + (8 -> Seq("x")) + (11 -> Seq("y"))
    val ruleMapProb2                              = initRuleMap.-(8).-(11)
    val candidatesProb2                           = eval(0, initMapValuesProb2)(ruleMapProb2)._1

    println(messages.count(msg => candidatesProb2.exists(p => matchPattern(msg, p))))

  }
}
