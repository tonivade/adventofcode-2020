package adventofcode

import scala.io.Source

object Day6 {

  def union(input: Array[Set[Char]]): String =
    input.reduce((a, b) => a.union(b)).mkString

  def intersect(input: Array[Set[Char]]): String =
    input.reduce((a, b) => a.intersect(b)).mkString

  val input = Source.fromFile("src/main/resources/day6.txt").mkString

  def parse(answers: String, parser: Array[Set[Char]] => String) = 
    answers.split("\\n\\n").map(_.trim()).map(_.split('\n').map(_.toSet)).map(parser)

  def count(answers: String, parser: Array[Set[Char]] => String) =
    parse(answers, parser).map(_.size).sum
}

object Day6Part1 extends App {
  import Day6._

  println("Day6 Part1")

  println(count(input, union))
}

object Day6Part2 extends App {
  import Day6._

  println("Day6 Part2")

  println(count(input, intersect))
}

object Day6Test extends App {
  import Day6._

  val testInput = """abc
                  |
                  |a
                  |b
                  |c
                  |
                  |ab
                  |ac
                  |
                  |a
                  |a
                  |a
                  |a
                  |
                  |b
                  """.stripMargin

  assert(count(testInput, union) == 11)
  assert(count(testInput, intersect) == 6)
}