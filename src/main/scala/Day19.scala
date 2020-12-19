package adventofcode

import adventofcode.Day19.OrRule
import scala.io.Source

object Day19 {

  sealed trait Rule {
    def applyTo(line: String): Option[Int]
  }

  case class Rules(map: Map[Int, Rule]) {
    def apply(number: Int): Rule = map(number)

    def applyTo(line: String): List[Int] = 
      map.map { 
        case (pos, rule) => 
          rule.applyTo(line) match {
            case Some(a) if a == line.size => Some(pos)
            case _ => None
          }
      }.flatMap(_.toList).toList

    def applyTo(num: Int)(line: String): Boolean = {
      val rule = map(num)

      rule.applyTo(line) match {
        case Some(a) if a == line.size => true
        case _ => false
      }
    }
  }

  class LazyRule(rule: => Rule) extends Rule {
    override def applyTo(line: String): Option[Int] = rule.applyTo(line)
  }

  object LazyRule {
    def apply(rule: => Rule): LazyRule = new LazyRule(rule)
  }

  case class OrRule(left: Rule, right: Rule) extends Rule {
    override def applyTo(line: String): Option[Int] = {
      left.applyTo(line) match {
        case None => right.applyTo(line) match {
          case None => None
          case x => x
        }
        case x => x
      }
    }
  }

  case class AndRule(left: Rule, right: Rule) extends Rule {
    override def applyTo(line: String): Option[Int] = {
      left.applyTo(line) match {
        case Some(a) => right.applyTo(line.drop(a)) match {
          case Some(b) => Some(a + b)
          case x => x
        }
        case x => x
      }
    }
  }

  case class SimpleRule(value: Char) extends Rule {
    override def applyTo(line: String): Option[Int] = 
      if (line.isEmpty || line.charAt(0) != value) 
        None
      else 
        Some(1)
  }

  def parseLine(line: String): (Int, String) = {
    val Array(pos, rule) = line.split(":")
    
    (pos.toInt, rule.trim())
  }

  def parseRule(rules: => Rules)(line: String): Rule = {
    val simpleRuleRegex = """"(\w)"""".r
    val refRegex = """(\d+)""".r
    val refOrRegex = """(\d+) \| (\d+) (\d+)""".r
    val andRuleRegex = """(\d+) (\d+)""".r
    val orRuleRegex = """(\d+) \| (\d+)""".r
    val andOrAndRuleRegex = """(\d+) (\d+) \| (\d+) (\d+)""".r
    val andOrAndAndRuleRegex = """(\d+) (\d+) \| (\d+) (\d+) (\d+)""".r

    val result = line match {
      case simpleRuleRegex(value) => SimpleRule(value.charAt(0))
      case andRuleRegex(a, b) => 
        AndRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt)))
      case orRuleRegex(a, b) => 
        OrRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt)))
      case andOrAndRuleRegex(a, b, c, d) => 
        OrRule(
          AndRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt))), 
          AndRule(LazyRule(rules(c.toInt)), LazyRule(rules(d.toInt))))
      case refOrRegex(a, b, c) => 
        OrRule(
          LazyRule(rules(a.toInt)), 
          AndRule(LazyRule(rules(b.toInt)), LazyRule(rules(b.toInt))))
      case refRegex(a) => LazyRule(rules(a.toInt))
      case andOrAndAndRuleRegex(a, b, c, d, e) => 
        OrRule(
          AndRule(LazyRule(rules(a.toInt)), LazyRule(rules(b.toInt))), 
          AndRule(LazyRule(rules(c.toInt)), 
            AndRule(LazyRule(rules(d.toInt)), LazyRule(rules(e.toInt)))))
      case _ => throw new IllegalArgumentException(s"`$line`")
    }

    result
  }

  def parse(input: String): Map[Int, String] =
    input.linesIterator.map(parseLine).toMap

  val Array(rulesPart, linesPart) = Source.fromResource("rules.txt").mkString.split("\\n\\n")
}

object Day19Part1 extends App {
  import Day19._

  val rules: Rules = Rules(parse(rulesPart).mapValues(parseRule(rules)).toMap)

  println(linesPart.linesIterator.filter(rules.applyTo(0)).size)
}

object Day19Part2 extends App {
  import Day19._

  val input = parse(rulesPart)
  val fixedInput = input + (8 -> "42 | 42 8") + (11 -> "42 31 | 42 11 31")
  val rules: Rules = Rules(fixedInput.mapValues(parseRule(rules)).toMap)

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

  val rules: Rules = Rules(parse(input).mapValues(parseRule(rules)).toMap)

  assert(rules.applyTo("aab") == List(0))
  assert(rules.applyTo("aba") == List(0))
  assert(rules.applyTo("aa").isEmpty)
  assert(rules.applyTo("ab") == List(2))

  println("OK")
}