package adventofcode

import scala.io.Source
import scala.annotation.tailrec
import scala.sys.process.processInternal

object Day13 {
  
  val input = Source.fromResource("buses.txt").getLines().toVector

  def parseBuses(line: String): Seq[Int] =
    line.split(",").filterNot(_ == "x").map(_.toInt)

  def parseBusesWithPosition(line: String): Seq[(Int, Int)] =
    line.split(",").zipWithIndex.filterNot(_._1 == "x").map(t => (t._1.toInt, t._2)).toVector

  def search(timestamp: Int, buses: Seq[Int]): (Int, Int) =
    buses.map(b => (b, b - timestamp % b)).minBy(_._2)

  // brute force algorithm
  @tailrec
  def search2(buses: Seq[(Int, Int)], m: BigInt = 1): BigInt = {
    val x = buses.head._1 * m
    
    val a = buses.tail.map(t => (x + t._2) % t._1).toVector

    if (a.forall(_ == 0)) 
      x
    else
      search2(buses, m + 1)
  }

  // based on this: https://www.freecodecamp.org/news/how-to-implement-the-chinese-remainder-theorem-in-java-db88a3f1ffe0/
  def search3(buses: Seq[(Int, Int)]): BigInt = {
    val product = buses.map(_._1).foldLeft(BigInt(1))(_ * _)

    val pproduct = buses.map(product / _._1)
    val reminder = buses.map { case (bus, pos) => bus - (pos % bus) }
    val inverse = (buses zip pproduct).map { case ((bus, _), pp) => computeInverse(pp, bus) }

    val sum = (pproduct zip reminder zip inverse).map { case ((pp, r), i) => pp * r * i }.sum

    sum.mod(product)
  }

  def computeInverse(i: BigInt, j: BigInt): BigInt = {
    @tailrec
    def loop(a: BigInt, b: BigInt, x: BigInt, y: BigInt, r: BigInt, s: BigInt): BigInt =
      if (b != 0) {
        val q = a / b

        loop(b, a % b, r, s, x - q * r, y - q * s)
      } else x

    loop(i, j, 1, 0, 0, 1)
  }
}

object Day13Part1 extends App {
  import Day13._

  val timestamp = input(0).toInt
  val buses = parseBuses(input(1))
  
  val (busId, min) = search(timestamp, buses)

  println(busId * min)
}

object Day13Part2 extends App {
  import Day13._

  val buses = parseBusesWithPosition(input(1))

  println(search3(buses))
}

object Day13Test extends App {
  import Day13._

  val input1 = "7,13,x,x,59,x,31,19"
  val timestamp = 939
  val buses = parseBuses(input1)

  val (busId, min) = search(timestamp, buses)

  assert(busId * min == 295)

  val busesWithPosition = parseBusesWithPosition(input1)

  assert(search3(busesWithPosition) == 1068781)
  assert(search3(parseBusesWithPosition("17,x,13,19")) == 3417)
  assert(search3(parseBusesWithPosition("67,7,59,61")) == 754018)
  assert(search3(parseBusesWithPosition("67,x,7,59,61")) == 779210)
  assert(search3(parseBusesWithPosition("67,7,x,59,61")) == 1261476)
  assert(search3(parseBusesWithPosition("1789,37,47,1889")) == 1202161486)

  println("OK")
}