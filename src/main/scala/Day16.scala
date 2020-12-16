package adventofcode

import scala.io.Source

object Day16 {

  case class Rule(name: String, first: Range, last: Range) {
    def apply(value: Int): Boolean =
      first.contains(value) || last.contains(value)
  }

  def parseRule(input: String): Rule = {
    val ruleregex = """([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

    input match {
      case ruleregex(name, firstFrom, firstTo, lastFrom, lastTo) => 
        Rule(name, firstFrom.toInt to firstTo.toInt, lastFrom.toInt to lastTo.toInt)
      case _ => 
        throw new RuntimeException(s"invalid $input")
    }
  }

  def parseTicket(input: String): Seq[Int] =
    input.split(",").map(_.toInt).toVector

  def applyRules(rules: Seq[Rule], ticket: Seq[Int]): Seq[Int] =
    ticket.filterNot(v => rules.exists(_ apply v))

  def errorRate(rules: Seq[Rule], tickets: Seq[Seq[Int]]): Int =
    tickets.flatMap(applyRules(rules, _)).sum
}

object Day16Part1 extends App {
  import Day16._

  val input = Source.fromResource("tickets.txt").mkString.split("\\n\\n")

  val rules = input(0).split("\n").map(parseRule).toVector
  val yours = input(1).split("\n").tail.map(parseTicket).head
  val nearby = input(2).split("\n").tail.map(parseTicket).toVector

  println(errorRate(rules, nearby))
}