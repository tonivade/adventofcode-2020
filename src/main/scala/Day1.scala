package adventofcode

import scala.io.Source

object Day1Util {
   
  val numbers = Source.fromFile("src/main/resources/input.txt").getLines().map(_.toInt).toList

  def combinations(number: Int, sum: Int): List[Int] = 
    numbers.combinations(number).find(x => x.fold(0)(_ + _) == sum).orNull
}

object Day1Part1 extends App {

  println("Day1 Part1")

  val result = Day1Util.combinations(2, 2020)

  println(s"values ${result(0)} and ${result(1)} = ${result(0) * result(1)}")
}

object Day1Part2 extends App {

  println("Day1 Part2")

  val result = Day1Util.combinations(3, 2020)

  println(s"values ${result(0)} and ${result(1)} and ${result(2)} = ${result(0) * result(1) * result(2)}")
}