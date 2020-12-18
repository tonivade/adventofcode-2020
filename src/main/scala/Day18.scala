package adventofcode

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day18 {

  sealed trait Expr {
    def eval: Long
  }
  
  case class Sum(a: Expr, b: Expr) extends Expr {
    override def eval: Long = a.eval + b.eval
  }

  case class Pro(a: Expr, b: Expr) extends Expr {
    override def eval: Long = a.eval * b.eval
  }

  case class Num(a: Long) extends Expr {
    override def eval: Long = a
  }

  def toRPN(input: String): String = {
    val split = input.split(' ').flatMap(_.toCharArray()).reverse

    val (out, op) = split.foldLeft((ListBuffer.empty[Char], Stack.empty[Char])) {
      case ((output, operator), current) if current.isDigit => ((output :+ current), operator)
      case ((output, operator), '+') => (output, operator.push('+'))
      case ((output, operator), '*') => (output, operator.push('*'))
      case ((output, operator), ')') => (output, operator.push('('))
      case ((output, operator), '(') => {
        while (operator.top != '(') {
          output += operator.pop
        }
        operator.pop
        (output, operator)
      }
    }

    (out ++ op).mkString(" ")
  }

  def toRPN2(input: String): String = {
    val split = input.split(' ').flatMap(_.toCharArray()).reverse

    val (out, op) = split.foldLeft((ListBuffer.empty[Char], Stack.empty[Char])) {
      case ((output, operator), current) if current.isDigit => ((output :+ current), operator)
      case ((output, operator), '+') => (output, operator.push('+'))
      case ((output, operator), '*') => {
        while (!operator.isEmpty && operator.top == '+') {
          output += operator.pop
        }
        (output, operator.push('*'))
      }
      case ((output, operator), ')') => (output, operator.push('('))
      case ((output, operator), '(') => {
        while (operator.top != '(') {
          output += operator.pop
        }
        operator.pop
        (output, operator)
      }
    }

    (out ++ op).mkString(" ")
  }

  def parse(input: String): Expr = {
    val split = input.split(' ').map(_.charAt(0))

    val res = split.foldLeft(Stack.empty[Expr]) {
      case (stack, current) if current.isDigit => stack.push(Num(current.toInt - '0'.toInt))
      case (stack, '+') => stack.push(Sum(stack.pop, stack.pop))
      case (stack, '*') => stack.push(Pro(stack.pop, stack.pop))
    }

    res.pop
  }
}

object Day18Part1 extends App {
  import Day18._

  val input = Source.fromResource("homework.txt").getLines().map(toRPN).map(parse)

  println(input.map(_.eval).foldLeft(BigInt(0))(_ + _))
}

object Day18Part2 extends App {
  import Day18._

  val input = Source.fromResource("homework.txt").getLines().map(toRPN2).map(parse)

  println(input.map(_.eval).foldLeft(BigInt(0))(_ + _))
}

object Day18Test extends App {
  import Day18._

  assert(parse(toRPN("1 + 2 * 3 + 4 * 5 + 6")).eval == 71)
  assert(parse(toRPN("((((1 + 2) * 3) + 4) * 5) + 6")).eval == 71)
  assert(parse(toRPN("1 + (2 * 3) + (4 * (5 + 6))")).eval == 51)
  assert(parse(toRPN("(1 + 2 * 3) + (4 * (5 + 6))")).eval == 53)

  // 1 + 2 * 3 + 4 * 5 + 6
  // 1 2 + 3 * 4 + 5 * 6 +
  val x1 = Sum(Num(1), Num(2))
  val x2 = Pro(x1, Num(3))
  val x3 = Sum(x2, Num(4))
  val x4 = Pro(x3, Num(5))
  val x5 = Sum(x4, Num(6))
  
  assert(x5.eval == 71)
  assert(parse("1 2 + 3 * 4 + 5 * 6 +").eval == 71)
  assert(parse("6 5 4 3 2 1 + * + * +").eval == 71)

  // 1 + (2 * 3) + (4 * (5 + 6))
  // 1 2 3 * + 4 5 6 + * +
  val y1 = Pro(Num(2), Num(3))
  val y2 = Sum(Num(5), Num(6))
  val y3 = Pro(Num(4), y2)
  val y4 = Sum(Num(1), y1)
  val y5 = Sum(y4, y3)

  assert(y5.eval == 51)
  assert(parse("1 2 3 * + 4 5 6 + * +").eval == 51)
  assert(parse("1 2 3 * 4 5 6 + * + +").eval == 51)
  
  assert(parse(toRPN2("1 + 2 * 3 + 4 * 5 + 6")).eval == 231)
  assert(parse(toRPN2("((((1 + 2) * 3) + 4) * 5) + 6")).eval == 71)
  assert(parse(toRPN2("1 + (2 * 3) + (4 * (5 + 6))")).eval == 51)
  assert(parse(toRPN2("(1 + 2 * 3) + (4 * (5 + 6))")).eval == 53)

  println("OK")
}