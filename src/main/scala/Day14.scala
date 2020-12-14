package adventofcode

import scala.io.Source

object Day14 {

  sealed trait Op
  case class Mask(zero: BigInt, one: BigInt, floating: Seq[Int]) extends Op {

    def applyToValue(value: BigInt): BigInt = (value & zero) | one

    def applyToAddress(address: BigInt): Seq[BigInt] = {
      val binary = (address | one).toString(2).reverse.zipWithIndex.map { case (bit, pos) => (pos, bit.asDigit) }.toMap

      val updates = (0 until (1 << floating.size)).map { x =>
        val current = x.toBinaryString.reverse.padTo(floating.size, '0').zipWithIndex.map { case (bit, pos) => (pos, bit.asDigit) }.toMap
        floating.reverse.zipWithIndex.map { case (p, i) => p -> current.getOrElse(i, 0) }
      }

      updates.map(binary ++ _).map(mapToInt)
    }
  }

  def mapToInt(map: Map[Int, Int]): BigInt = {
    val x = (0 to map.keys.max).map(map.getOrElse(_, 0)).reverse.mkString

    BigInt(x, 2)
  }

  case class Set(position: BigInt, value: BigInt) extends Op

  type Memory = Map[BigInt, BigInt]

  object Mask {
    val empty = Mask((2 ^ 36) - 1, 0, List.empty)
  }

  object Memory {
    val empty = Map.empty[BigInt, BigInt]
  }

  def parseMask(mask: String): Mask = {
    val (zero, one, floating) = mask.zipWithIndex.foldLeft(("", "", List.empty[Int])) {
      case ((zeros, ones, floating), ('0', i)) => (zeros + '0', ones + '0', floating)
      case ((zeros, ones, floating), ('1', i)) => (zeros + '1', ones + '1', floating)
      case ((zeros, ones, floating), ('X', i)) => (zeros + '1', ones + '0', floating :+ (35 - i))
      case _ => throw new IllegalArgumentException(s"invalid mask $mask")
    }
    Mask(BigInt(zero, 2), BigInt(one, 2), floating)
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
      case ((mem, current), Set(pos, value)) => (mem + (pos -> current.applyToValue(value)), current)
    }

    result
  }

  def run2(program: Vector[Op]): Memory = {
    val (result, _) = program.foldLeft((Memory.empty, Mask.empty)) {
      case ((mem, _), mask: Mask) => (mem, mask)
      case ((mem, current), Set(pos, value)) => {
        val writes = current.applyToAddress(pos).map(_ -> value)
        (mem ++ writes, current)
      }
    }

    result
  }
  
  val input = Source.fromResource("mask.txt").getLines().map(parseLine).toVector
}

object Day14Part1 extends App {
  import Day14._

  println(run(input).values.sum)
}

object Day14Part2 extends App {
  import Day14._

  println(run2(input).values.sum)
}

object Day14Test extends App {
  import Day14._

  assert(Mask.empty.applyToValue(1) == 1)
  assert(Mask.empty.applyToValue(0) == 0)

  val input1 = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                 |mem[8] = 11
                 |mem[7] = 101
                 |mem[8] = 0""".stripMargin
  val program1 = input1.linesIterator.map(parseLine).toVector
  assert(run(program1).values.sum == 165)

  val input2 = """mask = 000000000000000000000000000000X1001X
                 |mem[42] = 100
                 |mask = 00000000000000000000000000000000X0XX
                 |mem[26] = 1""".stripMargin
  val program2 = input2.linesIterator.map(parseLine).toVector
  assert(run2(program2).values.sum == 208)

  println("OK")
}