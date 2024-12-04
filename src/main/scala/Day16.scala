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

  def parse(input: String): (Seq[Rule], Seq[Int], Seq[Seq[Int]]) = {
    val split = input.split("\\n\\n")

    val rules = split(0).split("\n").map(parseRule).toVector
    val yours = split(1).split("\n").tail.map(parseTicket).head
    val nearby = split(2).split("\n").tail.map(parseTicket).toVector

    (rules, yours, nearby)
  }

  def parseTicket(input: String): Seq[Int] =
    input.split(",").map(_.toInt).toVector

  def applyRules(rules: Seq[Rule], ticket: Seq[Int]): Seq[Int] =
    ticket.filterNot(v => rules.exists(_ apply v))

  def errorRate(rules: Seq[Rule], tickets: Seq[Seq[Int]]): Int =
    tickets.flatMap(applyRules(rules, _)).sum

  def checkRules(rules: Seq[Rule], nearby: Seq[Seq[Int]]): Map[Int, String] = {

    val validTickets = nearby.filter(applyRules(rules, _).isEmpty)

    val maps = validTickets.map { t => t.zipWithIndex.map(_.swap).toMap }.foldLeft(Map.empty[Int, List[Int]]) {
      case (result, current) => {
        current.toSeq.map {
          case (pos, value) => (pos, value :: result.getOrElse(pos, Nil))
        }.toMap
      }
    }

    val validation = maps.toSeq.map {
      case (pos, values) => {
        (pos, values.flatMap {
          v => rules.map {
            r => (r.name, r.apply(v))
          }
        }.groupBy(_._1).mapValues(_.forall(_._2)))
      }
    }

    val grouped = validation.map {
      case (pos, values) => (pos, values.filter(_._2).map(_._1).toList)
    }.sortBy(_._2.size)

    val result = grouped.foldLeft((Map.empty[Int, String], Set.empty[String])) {
      case ((map, assigned), (pos, list)) => 
        val field = list.filterNot(assigned.contains(_)).head
        (map + (pos -> field), assigned + field)
    }

    result._1
  }

  def readTicket(fields: Map[Int, String], ticket: Seq[Int]): Map[String, Int] =
    fields.map { case (pos, name) => (name, ticket(pos)) }.toMap

  val input = Source.fromResource("day16.txt").mkString

  val (rules, yours, nearby) = parse(input)
}

object Day16Part1 extends App {
  import Day16._

  println(errorRate(rules, nearby))
}

object Day16Part2 extends App {
  import Day16._

  val checked = checkRules(rules, nearby)
  val ticket = readTicket(checked, yours)

  println(ticket.filter(_._1.startsWith("departure")).map(_._2).foldLeft(1L)(_ * _))
}

object Day16Test extends App {
  import Day16._

  val input1 = """class: 0-1 or 4-19
                 |row: 0-5 or 8-19
                 |seat: 0-13 or 16-19
                 |
                 |your ticket:
                 |11,12,13
                 |
                 |nearby tickets:
                 |3,9,18
                 |15,1,5
                 |5,14,9""".stripMargin

  val (rules1, yours1, tickets1) = parse(input1)

  val result = checkRules(rules1, tickets1)
  assert(result == Map(0 -> "row", 1 -> "class", 2 -> "seat"))
  assert(readTicket(result, yours1) == Map("row" -> 11, "class" -> 12, "seat" -> 13))

  println("OK")
}