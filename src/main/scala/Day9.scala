package adventofcode

import scala.io.Source

object Day9 {
  def isValid(numbers: Array[Long]): Boolean = {
    val last = numbers.last
    val preamble = numbers.dropRight(1)
    preamble.combinations(2).map(_.sum).exists(_ == last)
  }

  def findInvalid(input: Array[Long], preambleSize: Int): List[Long] =
    input.sliding(preambleSize + 1).filterNot(isValid).map(_.last).toList
}

object Day9Part1 extends App {
  import Day9._

  println("Day9 Part1")

  val input = Source.fromFile("src/main/resources/encrypted.txt").getLines().map(_.toLong).toArray

  println(findInvalid(input, 25).head)
}

object Day9Test extends App {
  import Day9._

  val input = """35
                |20
                |15
                |25
                |47
                |40
                |62
                |55
                |65
                |95
                |102
                |117
                |150
                |182
                |127
                |219
                |299
                |277
                |309
                |576""".stripMargin

  val encrypted = input.split('\n').map(_.toLong)

  val result = findInvalid(encrypted, 5)

  assert(result.head == 127)

  println("OK")
}