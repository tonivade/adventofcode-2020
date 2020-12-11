package adventofcode

import scala.io.Source

object Day10 {

  def find(adapters: List[Int]): List[(Int, List[Int])] = {
    val sortedAdapters = 0 :: adapters.sorted
    sortedAdapters
      .map(i => (i, sortedAdapters.filter(_ > i).filter(_ < i + 4)))
  }

  def findAdapters(adapters: List[Int]): Map[Int, Int] =
    find(adapters)
      .map(t => (t._1, t._2.headOption.getOrElse(t._1 + 3)))
      .map(t => t._2 - t._1)
      .groupBy(identity)
      .map(t => (t._1, t._2.size))
      .toMap

  // impossible to compute all combinations for a real input
  def arrangeAdapters(adapters: List[Int]): Set[Set[Int]] = {
    val all = find(adapters).map(_._2).dropRight(1)

    all.foldLeft(Set(Set.empty[Int]))((a, e) => a.flatMap(i => e.map(j => i ++ Set(j))))
  }

  def arrangeAdapters2(adapters: List[Int]): Long = {
    val result = adapters.sorted.foldLeft(Map(0 -> 1L)) {
      case (counts, next) =>
        val count = (next - 3 to next - 1).map(counts.getOrElse(_, 0L)).sum
        counts + (next -> count)
    }
    result(adapters.max)
  }

  val input = Source.fromResource("jolts.txt").getLines().map(_.toInt).toList
}

object Day10Part1 extends App {
  import Day10._

  println("Day10 Part1")

  println(findAdapters(input).values.product)
}

object Day10Part2 extends App {
  import Day10._

  println("Day10 Part2")

  println(arrangeAdapters2(input))
}

object Day10Test extends App {
  import Day10._
  
  var input1 = """16
                 |10
                 |15
                 |5
                 |1
                 |11
                 |7
                 |19
                 |6
                 |12
                 |4""".stripMargin.linesIterator.map(_.toInt).toList

  val result1 = findAdapters(input1)

  assert(result1(1) == 7)
  assert(result1(3) == 5)
  assert(arrangeAdapters(input1).size == 8)
  assert(arrangeAdapters2(input1) == 8)

  val input2 = """28
                 |33
                 |18
                 |42
                 |31
                 |14
                 |46
                 |20
                 |48
                 |47
                 |24
                 |23
                 |49
                 |45
                 |19
                 |38
                 |39
                 |11
                 |1
                 |32
                 |25
                 |35
                 |8
                 |17
                 |7
                 |9
                 |4
                 |2
                 |34
                 |10
                 |3""".stripMargin.linesIterator.map(_.toInt).toList

  var result2 = findAdapters(input2)

  assert(result2(1) == 22)
  assert(result2(3) == 10)
  assert(arrangeAdapters(input2).size == 19208)
  assert(arrangeAdapters2(input2) == 19208)

  println("OK")
}