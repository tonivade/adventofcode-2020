package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day8 {

  sealed trait Op
  case object NoOp extends Op
  case class Acc(value: Int) extends Op
  case class Jmp(value: Int) extends Op

  type Program = Array[Op]

  val Program = Array

  def parseProgram(input: String): Program =
    input.split('\n').map(parseLine).toArray

  def parseLine(line: String): Op = {
    val split = line.splitAt(3)

    split._1 match {
      case "nop" => NoOp
      case "acc" => Acc(split._2.trim().toInt)
      case "jmp" => Jmp(split._2.trim().toInt)
    }
  }

  @tailrec
  def runProgram(program: Program, acc: Int = 0, current: Int = 0, visited: Set[Int] = Set.empty): Int =
    if (visited.contains(current))
      acc
    else
      program(current) match {
        case Acc(value) => runProgram(program, acc + value, current + 1, visited + current)
        case Jmp(value) => runProgram(program, acc, current + value, visited + current)
        case NoOp => runProgram(program, acc, current + 1, visited + current)
      }
}

object Day8Part1 extends App {
  import Day8._

  println("Day8 Part1")

  val program = parseProgram(Source.fromFile("src/main/resources/boot.txt").mkString)

  println(runProgram(program))
}

object Day8Test extends App {
  
  import Day8._

  val input = """nop +0
                |acc +1
                |jmp +4
                |acc +3
                |jmp -3
                |acc -99
                |acc +1
                |jmp -4
                |acc +6""".stripMargin

  var program = parseProgram(input)

  assert(runProgram(program) == 5)

  println("OK")
}