package adventofcode

import adventofcode.Day19.OrRule
import scala.io.Source

object Day19 {

  sealed trait Rule {
    def applyTo(line: String): Option[Int]
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

  case class Rules(map: Map[Int, Rule]) {

    def apply(number: Int): Rule = LazyRule(map(number))

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
        AndRule(rules(a.toInt), rules(b.toInt))
      case orRuleRegex(a, b) => 
        OrRule(rules(a.toInt), rules(b.toInt))
      case andOrAndRuleRegex(a, b, c, d) => 
        OrRule(
          AndRule(rules(a.toInt), rules(b.toInt)), 
          AndRule(rules(c.toInt), rules(d.toInt)))
      case refOrRegex(a, b, c) => 
        OrRule(
          rules(a.toInt), 
          AndRule(rules(b.toInt), rules(b.toInt)))
      case refRegex(a) => rules(a.toInt)
      case andOrAndAndRuleRegex(a, b, c, d, e) => 
        OrRule(
          AndRule(rules(a.toInt), rules(b.toInt)), 
          AndRule(rules(c.toInt), 
            AndRule(rules(d.toInt), rules(e.toInt))))
      case _ => throw new IllegalArgumentException(s"`$line`")
    }

    result
  }

  def parse(input: String): Map[Int, String] =
    input.linesIterator.map(parseLine).toMap

  def search(rules: Rules, lines: String): Int =
    lines.linesIterator.filter(rules.applyTo(0)).size

  val Array(rulesPart, linesPart) = Source.fromResource("rules.txt").mkString.split("\\n\\n")
}

object Day19Part1 extends App {
  import Day19._

  val rules: Rules = Rules(parse(rulesPart).mapValues(parseRule(rules)).toMap)

  println(search(rules, linesPart))
}

object Day19Part2 extends App {
  import Day19._

  val input = parse(rulesPart)
  val fixedInput = input + (8 -> "42 | 42 8") + (11 -> "42 31 | 42 11 31")

  val rules: Rules = Rules(fixedInput.mapValues(parseRule(rules)).toMap)

  println(search(rules, linesPart))
}

object Day19Test extends App {
  import Day19._

  val rule1 = OrRule(SimpleRule('a'), SimpleRule('b'))

  assert(rule1.applyTo("a") == Some(1))
  assert(rule1.applyTo("b") == Some(1))
  assert(rule1.applyTo("c") == None)
  
  val rule2 = AndRule(SimpleRule('a'), SimpleRule('b'))
  
  assert(rule2.applyTo("ab") == Some(2))
  assert(rule2.applyTo("ba") == None)

  val rule3 = AndRule(
    SimpleRule('a'), 
    OrRule(
      AndRule(SimpleRule('a'), SimpleRule('b')),
      AndRule(SimpleRule('b'), SimpleRule('a'))))
  
  assert(rule3.applyTo("aba") == Some(3))
  assert(rule3.applyTo("aab") == Some(3))
  assert(rule3.applyTo("aa") == None)
  assert(rule3.applyTo("ab") == None)
  assert(rule3.applyTo("ba") == None)
    
  val input1 = """0: 1 2
                |1: "a"
                |2: 1 3 | 3 1
                |3: "b"""".stripMargin

  val rules1: Rules = Rules(parse(input1).mapValues(parseRule(rules1)).toMap)

  assert(rules1.applyTo("aab") == List(0))
  assert(rules1.applyTo("aba") == List(0))
  assert(rules1.applyTo("aa").isEmpty)
  assert(rules1.applyTo("ab") == List(2))

  val input2 = """42: 9 14 | 10 1
                  |9: 14 27 | 1 26
                  |10: 23 14 | 28 1
                  |1: "a"
                  |11: 42 31
                  |5: 1 14 | 15 1
                  |19: 14 1 | 14 14
                  |12: 24 14 | 19 1
                  |16: 15 1 | 14 14
                  |31: 14 17 | 1 13
                  |6: 14 14 | 1 14
                  |2: 1 24 | 14 4
                  |0: 8 11
                  |13: 14 3 | 1 12
                  |15: 1 | 14
                  |17: 14 2 | 1 7
                  |23: 25 1 | 22 14
                  |28: 16 1
                  |4: 1 1
                  |20: 14 14 | 1 15
                  |3: 5 14 | 16 1
                  |27: 1 6 | 14 18
                  |14: "b"
                  |21: 14 1 | 1 14
                  |25: 1 1 | 1 14
                  |22: 14 14
                  |8: 42
                  |26: 14 22 | 1 20
                  |18: 15 15
                  |7: 14 5 | 1 21
                  |24: 14 1""".stripMargin
  
  val fixed = parse(input2) + (8 -> "42 | 42 8") + (11 -> "42 31 | 42 11 31")
  val rules2: Rules = Rules(parse(input2).mapValues(parseRule(rules2)).toMap)
  val rules3: Rules = Rules(fixed.mapValues(parseRule(rules3)).toMap)

  val lines2 = """abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
                  |bbabbbbaabaabba
                  |babbbbaabbbbbabbbbbbaabaaabaaa
                  |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
                  |bbbbbbbaaaabbbbaaabbabaaa
                  |bbbababbbbaaaaaaaabbababaaababaabab
                  |ababaaaaaabaaab
                  |ababaaaaabbbaba
                  |baabbaaaabbaaaababbaababb
                  |abbbbabbbbaaaababbbbbbaaaababb
                  |aaaaabbaabaaaaababaa
                  |aaaabbaaaabbaaa
                  |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
                  |babaaabbbaaabaababbaabababaaab
                  |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin

  assert(search(rules2, lines2) == 3)
  assert(search(rules3, lines2) == 12)

  println("OK")
}