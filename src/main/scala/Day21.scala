package adventofcode

import scala.io.Source

object Day21 {

  case class Item(ingredients: List[String], allergens: List[String])

  def parseLine(line: String): Item = {
    val regex = """(.+) \(contains (.+)\)""".r

    line match {
      case regex(ingredients, allergens) => Item(ingredients.trim.split(" ").toList, allergens.split(",").map(_.trim).toList)
      case _ => throw new IllegalArgumentException(s"not valid: $line")
    }
  }

  def all(items: List[Item]): Set[String] = items.flatMap(_.ingredients).toSet

  def probableAllergens(items: List[Item]): Map[String, List[String]] = {
    val step1 = items.map { case Item(ingredients, allergens) =>

      val p = 1d / (ingredients.size - allergens.size + 1).toDouble
      
      allergens.map { (_, ingredients.map { (_, p) }.toMap) }.toMap
    }

    val step2 = step1.foldLeft(Map.empty[String, Map[String, Double]]) {
      case (state, current) => 
        (state.toSeq ++ current.toSeq).groupBy(_._1).map {
          case (allergen, values) => 
            (allergen, values.map(_._2).foldLeft(Map.empty[String, Double]) {
              case (total, ingredients) => 
                (total.toSeq ++ ingredients.toSeq).groupBy(_._1).map { 
                  case (i, v) => (i, v.map(_._2).sum) 
                }.toMap
            })
        }.toMap
    }

    val maxP = step2.mapValues(_.maxBy(_._2)._2)

    step2.map { 
      case (a, ingrs) => 
        (a, ingrs.filter { 
          case (i, v) => v == maxP(a) }.map(_._1).toList) 
        }
  }

  def calculate(probable: Map[String, List[String]]): Set[String] =
    probable.values.foldLeft(Set.empty[String]) {
      case (state, current) =>
        state ++ current
    }

  def count(items: Seq[Item], result: Set[String]) =
    result.toList.map(i => items.flatMap(_.ingredients).count(_ == i)).sum

  def assign(probable: Map[String, List[String]]): List[(String, String)] =
    probable.toSeq.sortBy(_._2.size).foldLeft(List.empty[(String, String)]) {
      case (state, (allergen, ingredients)) =>
        val asigned = state.map(_._2).toSet
        val result = ingredients.filterNot(asigned.contains(_))
        result match {
          case head :: Nil => state :+ (allergen, head)
          case _ => throw new IllegalArgumentException(s"$result")
        }
    }
}

object Day21Part1 extends App {
  import Day21._

  val items = Source.fromResource("ingredients.txt").getLines().map(parseLine).toList
  val probable = probableAllergens(items)
  val result = all(items) -- calculate(probable)
  println(count(items, result))

  println(assign(probable).sortBy(_._1).map(_._2).mkString(","))
}

object Day21Test extends App {
  import Day21._

  val example = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                  |trh fvjkl sbzzf mxmxvkd (contains dairy)
                  |sqjhc fvjkl (contains soy)
                  |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin

  val items = example.linesIterator.map(parseLine).toList

  // (dairy,List(mxmxvkd))
  // (fish,List(mxmxvkd, sqjhc))
  // (soy,List(sqjhc, fvjkl))
  val probable = probableAllergens(items)

  val result = all(items) -- calculate(probable)

  assert(result == Set("kfcds", "nhms", "sbzzf", "trh"))
  assert(count(items, result) == 5)

  assign(probable).foreach(println)

  println("OK")
}