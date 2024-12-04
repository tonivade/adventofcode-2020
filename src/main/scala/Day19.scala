package adventofcode

import scala.io.Source
import scala.collection.immutable.Stream.cons
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

object Day19 {

  sealed trait Rule 
  case class Ref(number: Int) extends Rule 
  case class Or(left: Rule, right: Rule) extends Rule
  case class And(left: Rule, right: Rule) extends Rule
  case class Lit(value: Char) extends Rule

  case class Rules(map: Map[Int, Rule]) {

    def apply(number: Int): Rule = map(number)

    def applyTo(line: String): List[Int] =
      map.filter { 
        case (pos, rule) => check(rule)(line)
      }.keys.toList

    def applyTo(number: Int)(line: String): Boolean = 
      check(map(number))(line)

    def check(rule: Rule)(line: String): Boolean =
      applyRule(rule, line)(this) match {
        case Some(a) if a == line.size => true
        case _ => false
      }
  }

  def applyRule(rule: Rule, line: String)(implicit rules: Rules = Rules(Map.empty)): Option[Int] =
    rule match {
      case Ref(n) => applyRule(rules(n), line)
      case Lit(c) => if (line.isEmpty || c != line.charAt(0)) None else Some(1)
      case And(left, right) => 
        applyRule(left, line) match {
          case Some(a) => applyRule(right, line.drop(a)) match {
            case Some(b) => Some(a + b)
            case b => b
          }
          case a => a
        }
      case Or(left, right) => 
        applyRule(left, line) match {
          case None => applyRule(right, line)
          case a => a
        }
    }

  def parseLine(line: String): (Int, String) = {
    val Array(pos, rule) = line.split(":")
    
    (pos.toInt, rule.trim())
  }

  def parseRule(line: String): Rule = {
    val simpleRuleRegex = """"(\w)"""".r
    val refRegex = """(\d+)""".r
    val andRuleRegex = """(\d+) (\d+)""".r
    val orRuleRegex = """(\d+) \| (\d+)""".r
    val andOrAndRuleRegex = """(\d+) (\d+) \| (\d+) (\d+)""".r

    val result = line match {
      case simpleRuleRegex(value) => Lit(value.charAt(0))
      case andRuleRegex(a, b) => 
        And(Ref(a.toInt), Ref(b.toInt))
      case orRuleRegex(a, b) => 
        Or(Ref(a.toInt), Ref(b.toInt))
      case andOrAndRuleRegex(a, b, c, d) => 
        Or(
          And(Ref(a.toInt), Ref(b.toInt)), 
          And(Ref(c.toInt), Ref(d.toInt)))
      case refRegex(a) => Ref(a.toInt)
      case _ => throw new IllegalArgumentException(s"`$line`")
    }

    result
  }

  def parseRules(input: String): Rules =
    Rules(input.linesIterator.map(parseLine).toMap.mapValues(parseRule))

  def search(rules: Rules, lines: String): Int =
    lines.linesIterator.filter(rules.applyTo(0)).size

  val Array(rulesPart, linesPart) = Source.fromResource("day19.txt").mkString.split("\\n\\n")

  def applyRule42(rules: Rules, line: String, consumed: Int = 0, seq: List[Int] = List.empty): Option[(Int, List[Int])] =
    applyRule(rules(42), line)(rules) match {
      case Some(a) => applyRule42(rules, line.drop(a), consumed + a, seq :+ 42)
      case None => if (consumed > 0) Some(consumed, seq) else None
    }

  def applyRule31(rules: Rules, line: String, consumed: Int = 0, seq: List[Int] = List.empty): Option[(Int, List[Int])] =
    applyRule(rules(31), line)(rules) match {
      case Some(a) => applyRule31(rules, line.drop(a), consumed + a, seq :+ 31)
      case None => if (consumed > 0) Some(consumed, seq) else None
    }

  def apply42n31(rules: Rules, line: String, consumed: Int = 0, seq: List[Int] = List.empty): Option[(Int, List[Int])] = {
    val x = applyRule42(rules, line) match {
      case None => None
      case Some((a, s)) => applyRule31(rules, line.drop(a), a, s)
    }

    x match {
      case Some((a, s)) => apply42n31(rules, line.drop(a), consumed + a, seq ++ s)
      case None => if (consumed > 0) Some(consumed, seq) else None
    }
  }

  /*
   * applied a plan B
   * the result should be a sequence of 42 and 31
   * - it should start by pair of 42
   * - it should end by a 31
   * - it should be more 42 than 31 overall
   * - and in the rest of the elements:
   *  - if it have at least a 31 it should end with a 31 
   */
  def search2(rules: Rules, lines: String): Int = {
    val all = lines.linesIterator.map { line =>
      apply42n31(rules, line) match {
        case Some((a, seq)) if a == line.size => 
          val res1 = seq.last == 31 && seq.head == 42 && seq.tail.head == 42 && seq.count(_ == 42) > seq.count(_ == 31)
          val inner = seq.tail.tail.dropRight(1)
          val res2 = inner.count(_ == 31) > 0 && inner.last != 31
          res1 && !res2
        case Some((a, seq)) => false
        case _ => false
      }
    }

    all.count(_ == true)
  }
}

object Day19Part1 extends App {
  import Day19._

  val rules = parseRules(rulesPart)

  println(search(rules, linesPart))
}

object Day19Part2 extends App {
  import Day19._

  val rules = parseRules(rulesPart)

  println(search2(rules, linesPart))
}

object Day19Test extends App {
  import Day19._

  val rule1 = Or(Lit('a'), Lit('b'))

  assert(applyRule(rule1, "a") == Some(1))
  assert(applyRule(rule1, "b") == Some(1))
  assert(applyRule(rule1, "c") == None)

  val rule2 = And(Lit('a'), Lit('b'))
  
  assert(applyRule(rule2, "ab") == Some(2))
  assert(applyRule(rule2, "ba") == None)
  assert(applyRule(rule2, "a") == None)

  val rule3 = And(Lit('a'), Or(And(Lit('a'), Lit('b')), And(Lit('b'), Lit('a'))))
  
  assert(applyRule(rule3, "aba") == Some(3))
  assert(applyRule(rule3, "aab") == Some(3))
  assert(applyRule(rule3, "aa") == None)
  assert(applyRule(rule3, "ab") == None)
  assert(applyRule(rule3, "ba") == None)
    
  val input1 = """0: 1 2
                |1: "a"
                |2: 1 3 | 3 1
                |3: "b"""".stripMargin

  val rules1: Rules = parseRules(input1)

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
  
  val rules2 = parseRules(input2)

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
  assert(search2(rules2, lines2) == 12)

  println("OK")
}