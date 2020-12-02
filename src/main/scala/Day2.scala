package adventofcode

import scala.io.Source

case class Password(min: Int, max: Int, letter: Char, password: String) {
  lazy val isValidPart1: Boolean = {
    val chars = password.count(_ == letter)

    chars >= min && chars <= max
  }

  lazy val isValidPart2: Boolean = {
    var first = password(min - 1)
    var last = password(max - 1)
    
    first == letter ^ last == letter
  }
}

object Day2Util {

  def parseLine(line: String): Password = {
    val split = line.split(':')

    val rule = split(0).trim()
    val password = split(1).trim()

    val ruleSplit = rule.split(' ')

    val range = ruleSplit(0)
    val letter = ruleSplit(1)(0)

    val rangeSplit = range.split('-')

    val min = rangeSplit(0).toInt
    val max = rangeSplit(1).toInt

    Password(min, max, letter, password)
  }

  val passwords = Source.fromFile("src/main/resources/passwords.txt").getLines().map(parseLine).toList
  
  def filter(filter: Password => Boolean) = passwords.filter(filter)
}

object Day2Part1 extends App {

  println("Day2 Part1")

  val passwords = Day2Util.filter(_.isValidPart1)

  println(passwords.size)

}

object Day2Part2 extends App {

  println("Day2 Part2")

  val passwords = Day2Util.filter(_.isValidPart2)

  println(passwords.size)

}