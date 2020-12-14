package adventofcode

import scala.io.Source

object Day14 {

  sealed trait Op
  case class Mask(zero: BigInt, one: BigInt) extends Op {
    def apply(value: BigInt) = (value & zero) | one
  }
  case class Set(position: BigInt, value: BigInt) extends Op

  type Memory = Map[BigInt, BigInt]

  object Mask {
    val empty = Mask((2 ^ 36) - 1, 0)
  }

  object Memory {
    val empty = Map.empty[BigInt, BigInt]
  }

  def parseMask(mask: String): Mask = {
    val (zero, one) = mask.foldLeft(("", "")) {
      case ((zeros, ones), '0') => (zeros + '0', ones + '0')
      case ((zeros, ones), '1') => (zeros + '1', ones + '1')
      case ((zeros, ones), 'X') => (zeros + '1', ones + '0')
      case _ => throw new IllegalArgumentException(s"invalid mask $mask")
    }
    Mask(BigInt(zero, 2), BigInt(one, 2))
  }

  def parseLine(line: String): Op = {
    val maskregex = """mask = ([X01]{36})""".r
    val memregex = """mem\[(\d+)\] = (\d+)""".r

    line match {
      case maskregex(mask) => parseMask(mask)
      case memregex(position, value) => Set(position.toLong, value.toLong)
    }
  }

  def run(program: Vector[Op]): Memory = {
    val (result, _) = program.foldLeft((Memory.empty, Mask.empty)) {
      case ((mem, _), mask: Mask) => (mem, mask)
      case ((mem, current), Set(pos, value)) => (mem + (pos -> current(value)), current)
    }

    result
  }
}

object Day14Part1 extends App {
  import Day14._

  val input = Source.fromResource("mask.txt").getLines().map(parseLine).toVector

  println(run(input).values.sum)
}

object Day14Test extends App {
  import Day14._

  assert(Mask.empty(1) == 1)
  assert(Mask.empty(0) == 0)

  val input1 = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                 |mem[8] = 11
                 |mem[7] = 101
                 |mem[8] = 0""".stripMargin

  val program = input1.linesIterator.map(parseLine).toVector

  val result = run(program)

  assert(result.values.sum == 165)

  println("OK")
}