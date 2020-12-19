package adventofcode

import adventofcode.Day19.OrRule
import scala.io.Source

object Day19 {

  sealed trait Rule {
    def applyTo(line: String): (Int, Boolean)
  }

  case class Rules(map: Map[Int, Rule]) {
    def apply(number: Int): Rule = map(number)

    def applyTo(line: String): List[Int] = 
      map.map { 
        case (pos, rule) => 
          val (consumed, result) = rule.applyTo(line)
          (pos, consumed == line.size && result) 
      }.filter(_._2).map(_._1).toList

    def applyTo(num: Int)(line: String): Boolean = {
      val rule = map(num)

      val (consume, result) = rule.applyTo(line)
      
      consume == line.size && result
    }
  }

  class LazyRule(rule: => Rule) extends Rule {
    override def applyTo(line: String): (Int, Boolean) = rule.applyTo(line)
  }

  object LazyRule {
    def apply(rule: => Rule): LazyRule = new LazyRule(rule)
  }

  case class OrRule(left: Rule, right: Rule) extends Rule {
    override def applyTo(line: String): (Int, Boolean) = {
      val (consumedLeft, resultLeft) = left.applyTo(line)
      val (consumedRight, resultRight) = right.applyTo(line)

      if (resultLeft)
        (consumedLeft, resultLeft)
      else if (resultRight)
        (consumedRight, resultRight)
      else
        (0, false)
    }
  }

  case class AndRule(left: Rule, right: Rule) extends Rule {
    override def applyTo(line: String): (Int, Boolean) = {
      val (consumedLeft, resultLeft) = left.applyTo(line)
      if (resultLeft) {
        val (consumedRight, resultRight) = right.applyTo(line.drop(consumedLeft))

        (consumedLeft + consumedRight, resultRight)
      } else 
        (0, false)
    }
  }

  case class SimpleRule(value: Char) extends Rule {
    override def applyTo(line: String): (Int, Boolean) = 
      if (line.isEmpty || line.charAt(0) != value) 
        (0, false) 
      else 
        (1, true)
  }

  def parseRule(rules: => Rules)(line: String): (Int, Rule) = {
    val Array(pos, rule) = line.split(":")

    val simpleRuleRegex = """"(\w)"""".r
    val refRegex = """(\d+)""".r
    val andRuleRegex = """(\d+) (\d+)""".r
    val orRuleRegex = """(\d+) \| (\d+)""".r
    val andOrAndRuleRegex = """(\d+) (\d+) \| (\d+) (\d+)""".r

    val result = rule.trim match {
      case simpleRuleRegex(value) => SimpleRule(value.charAt(0))
      case andRuleRegex(a, b) => 
        AndRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt)))
      case orRuleRegex(a, b) => 
        OrRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt)))
      case andOrAndRuleRegex(a, b, c, d) => 
        OrRule(
          AndRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt))), 
          AndRule(LazyRule(rules(c.toInt)), LazyRule(rules(d.toInt))))
      case refRegex(a) => LazyRule(rules(a.toInt))
      case _ => throw new IllegalArgumentException(s"`${rule.trim}`")
    }

    (pos.toInt, result)
  }
}

object Day19Part1 extends App {
  import Day19._

  val Array(rulesPart, linesPart) = Source.fromResource("rules.txt").mkString.split("\\n\\n")

  val rules: Rules = Rules(rulesPart.linesIterator.map(parseRule(rules)).toMap)

  println(linesPart.linesIterator.filter(rules.applyTo(0)).size)
}

object Day19Test extends App {
  import Day19._

  val rule1 = OrRule(SimpleRule('a'), SimpleRule('b'))

  assert(rule1.applyTo("a") == (1, true))
  assert(rule1.applyTo("b") == (1, true))
  assert(rule1.applyTo("c") == (0, false))
  
  val rule2 = AndRule(SimpleRule('a'), SimpleRule('b'))
  
  assert(rule2.applyTo("ab") == (2, true))
  assert(rule2.applyTo("ba") == (0, false))

  val rule3 = AndRule(
    SimpleRule('a'), 
    OrRule(
      AndRule(SimpleRule('a'), SimpleRule('b')),
      AndRule(SimpleRule('b'), SimpleRule('a'))))
  
  assert(rule3.applyTo("aba") == (3, true))
  assert(rule3.applyTo("aab") == (3, true))
  assert(rule3.applyTo("aa") == (1, false))
  assert(rule3.applyTo("ab") == (1, false))
  assert(rule3.applyTo("ba") == (0, false))
    
  val input = """0: 1 2
                |1: "a"
                |2: 1 3 | 3 1
                |3: "b"""".stripMargin

  val rules: Rules = Rules(input.linesIterator.map(parseRule(rules)).toMap)

  println(rules.applyTo("aab"))
  println(rules.applyTo("aba"))
  println(rules.applyTo("aa"))
  println(rules.applyTo("ab"))

  println("OK")
}