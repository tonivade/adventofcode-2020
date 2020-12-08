package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day8 {

  sealed trait Op
  case object NoOp extends Op
  case class Acc(value: Int) extends Op
  case class Jmp(value: Int) extends Op

  type Program = Vector[Op]

  val Program = Vector

  def parseProgram(input: String): Program =
    input.split('\n').map(parseLine).toVector

  def parseLine(line: String): Op = {
    val split = line.splitAt(3)

    split._1 match {
      case "nop" => NoOp
      case "acc" => Acc(split._2.trim().toInt)
      case "jmp" => Jmp(split._2.trim().toInt)
    }
  }

  @tailrec
  def runProgram(program: Program, acc: Int = 0, current: Int = 0, visited: Set[Int] = Set.empty): Either[Int, Int] =
    if (visited.contains(current))
      Left(acc)
    else if (current < program.size)
      program(current) match {
        case Acc(value) => runProgram(program, acc + value, current + 1, visited + current)
        case Jmp(value) => runProgram(program, acc, current + value, visited + current)
        case NoOp => runProgram(program, acc, current + 1, visited + current)
      }
    else
      Right(acc)

  def fix(program: Program, position: Int): Program = program.updated(position, NoOp)

  def fixProgram(program: Program): Either[Int, Int] = 
    program.zipWithIndex.filter(_._1.isInstanceOf[Jmp]).map(_._2).map(fix(program, _)).map(runProgram(_)).filter(_.isRight).head

  val program = parseProgram(Source.fromFile("src/main/resources/boot.txt").mkString)
}

object Day8Part1 extends App {
  import Day8._

  println("Day8 Part1")

  println(runProgram(program))
}

object Day8Part2 extends App {
  import Day8._

  println("Day8 Part2")

  println(fixProgram(program))
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

  assert(runProgram(program) == Left(5))

  assert(fixProgram(program) == Right(8))

  println("OK")
}