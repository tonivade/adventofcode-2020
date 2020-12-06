package adventofcode

import scala.io.Source

object Day6 {

  def parseGroup(input: String): String =
    input.trim().split('\n').mkString.toSet.mkString

  val input = Source.fromFile("src/main/resources/customs.txt").mkString

  def parse(answers: String) = 
    answers.split("\\n\\n").map(parseGroup)

  def count(answers: String) =
    parse(answers).map(_.size).sum
}

object Day6Part1 extends App {
  import Day6._

  println("Day6 Part1")

  println(count(input))
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

  assert(count(testInput) == 11)
}