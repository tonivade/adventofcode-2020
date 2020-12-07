package adventofcode

import scala.io.Source

object Day7 {

  case class Bag(name: String, map: Map[String, Int]) {
    def contains(other: String): Boolean = map.contains(other)
  }

  def parseLine(line: String): Bag = {

    val noBagsPattern = "(\\w+ \\w+) bags contain no other bags\\.".r
    val bagPattern1 = 
      "(\\w+ \\w+) bags contain (?:([0-9]) (\\w+ \\w+) bags?\\.)".r
    val bagPattern2 = 
      "(\\w+ \\w+) bags contain (?:([0-9]) (\\w+ \\w+) bags?,) (?:([0-9]) (\\w+ \\w+) bags?\\.)".r
    val bagPattern3 = 
      "(\\w+ \\w+) bags contain (?:([0-9]) (\\w+ \\w+) bags?,) (?:([0-9]) (\\w+ \\w+) bags?,) (?:([0-9]) (\\w+ \\w+) bags?\\.)".r
    val bagPattern4 = 
      "(\\w+ \\w+) bags contain (?:([0-9]) (\\w+ \\w+) bags?,) (?:([0-9]) (\\w+ \\w+) bags?,) (?:([0-9]) (\\w+ \\w+) bags?,) (?:([0-9]) (\\w+ \\w+) bags?\\.)".r

    line match {
      case noBagsPattern(name) => Bag(name, Map())
      case bagPattern1(name, b, c) => 
        Bag(name, Map(c -> b.toInt))
      case bagPattern2(name, b, c, d, e) => 
        Bag(name, Map(c -> b.toInt, e -> d.toInt))
      case bagPattern3(name, b, c, d, e, f, g) => 
        Bag(name, Map(c -> b.toInt, e -> d.toInt, g -> f.toInt))
      case bagPattern4(name, b, c, d, e, f, g, h, i) => 
        Bag(name, Map(c -> b.toInt, e -> d.toInt, g -> f.toInt, i -> h.toInt))
    }
  }

  def search(name: String, bags: List[Bag]): Set[Bag] = {
    val found = bags.filter(_ contains name).toSet
      
    found ++ found.flatMap(b => search(b.name, bags))
  }

  def count(name: String, bags: Map[String, Bag]): Int =
    bags(name).map.foldLeft(1)((c, t) => c + (count(t._1, bags) * t._2))

  val input = Source.fromFile("src/main/resources/bags.txt").getLines().toList

}

object Day7Part1 extends App {
  import Day7._

  println("Day7 Part1")

  val bags = input.map(parseLine)

  println(search("shiny gold", bags).size)
}

object Day7Part2 extends App {
  import Day7._

  println("Day7 Part2")

  val bags = input.map(parseLine).map(b => (b.name, b)).toMap

  println(count("shiny gold", bags) - 1)
}

object Day7Test extends App {
  import Day7._

  val test = """light red bags contain 1 bright white bag, 2 muted yellow bags.
                |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
                |bright white bags contain 1 shiny gold bag.
                |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
                |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
                |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
                |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
                |faded blue bags contain no other bags.
                |dotted black bags contain no other bags.""".stripMargin

  val bags = test.split("\n").map(parseLine).toList

  assert(search("shiny gold", bags).size == 4)

  val test2 = """shiny gold bags contain 2 dark red bags.
                |dark red bags contain 2 dark orange bags.
                |dark orange bags contain 2 dark yellow bags.
                |dark yellow bags contain 2 dark green bags.
                |dark green bags contain 2 dark blue bags.
                |dark blue bags contain 2 dark violet bags.
                |dark violet bags contain no other bags.""".stripMargin
  
  val bags2 = test2.split("\n").map(parseLine).map(b => (b.name, b)).toMap

  assert(count("shiny gold", bags2) - 1 == 126)
}