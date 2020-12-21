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

  def allIngredients(items: List[Item]): Set[String] = items.flatMap(_.ingredients).toSet

  def probableAllergens(items: List[Item]): Map[String, List[String]] = {
    val rawP = items.map { case Item(ingredients, allergens) =>
      val p = 1d / (ingredients.size - allergens.size + 1).toDouble
      allergens.map { (_, ingredients.map { (_, p) }.toMap) }.toMap
    }

    val mergedP = rawP.reduce[Map[String, Map[String, Double]]](mergeMapOfMaps)

    val maxP = mergedP.mapValues(_.maxBy(_._2)._2)

    mergedP.map { 
      case (a, ingrs) => (a, ingrs.filter(_._2 == maxP(a)).map(_._1).toList) 
    }
  }

  def calculateAllergens(probable: Map[String, List[String]]): Set[String] =
    probable.values.foldLeft(Set.empty[String])(_ ++ _)

  def countIngredients(items: Seq[Item], result: Set[String]): Int =
    result.toList.map(i => items.flatMap(_.ingredients).count(_ == i)).sum

  def assignAllergens(probable: Map[String, List[String]]): List[(String, String)] =
    probable.toSeq.sortBy(_._2.size).foldLeft(List.empty[(String, String)]) {
      case (state, (allergen, ingredients)) =>
        val asigned = state.map(_._2).toSet
        val result = ingredients.filterNot(asigned.contains(_))
        result match {
          case head :: Nil => state :+ (allergen, head)
          case _ => throw new IllegalArgumentException(s"$result")
        }
    }

  def mergeMaps[K, V](map1: Map[K, V], map2: Map[K, V])(implicit num: Numeric[V]): Map[K, V] =
    (map1.toSeq ++ map2.toSeq).groupBy(_._1).map {
      case (k, values) => (k, values.map(_._2).sum)
    }
  
  def mergeMapOfMaps[K, V](map1: Map[K, Map[K, V]], map2: Map[K, Map[K, V]])(implicit num: Numeric[V]): Map[K, Map[K, V]] =
    (map1.toSeq ++ map2.toSeq).groupBy(_._1).map {
      case (k, values) => (k, values.map(_._2).reduce[Map[K, V]](mergeMaps))
    }
}

object Day21Part1 extends App {
  import Day21._

  val items = Source.fromResource("ingredients.txt").getLines().map(parseLine).toList
  val probable = probableAllergens(items)
  val result = allIngredients(items) -- calculateAllergens(probable)
  println(countIngredients(items, result))

  println(assignAllergens(probable).sortBy(_._1).map(_._2).mkString(","))
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

  val result = allIngredients(items) -- calculateAllergens(probable)

  assert(result == Set("kfcds", "nhms", "sbzzf", "trh"))
  assert(countIngredients(items, result) == 5)

  assignAllergens(probable).foreach(println)

  println("OK")
}