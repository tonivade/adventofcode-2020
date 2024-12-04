package adventofcode

import scala.io.Source
import scala.annotation.tailrec
import scala.sys.process.processInternal

object Day13 {
  
  val input = Source.fromResource("day13.txt").getLines().toVector

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
    
    val a = buses.tail.map { case (bus, pos) => (x + pos) % bus }.toVector

    if (a.forall(_ == 0)) 
      x
    else
      search2(buses, m + 1)
  }

  // based on this: https://www.freecodecamp.org/news/how-to-implement-the-chinese-remainder-theorem-in-java-db88a3f1ffe0/
  def search3(buses: Seq[(Int, Int)]): BigInt = {
    val product = buses.map(_._1).foldLeft(BigInt(1))(_ * _)

    val gdcs = buses.map {
      case (bus, pos) => {
        val pproduct = product / bus
        val remainder = bus - (pos % bus)
        val gdc = pproduct modInverse bus
        pproduct * remainder * gdc
      }
    }

    gdcs.sum mod product
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

  assert(search2(busesWithPosition) == 1068781)
  assert(search2(parseBusesWithPosition("17,x,13,19")) == 3417)
  assert(search2(parseBusesWithPosition("67,7,59,61")) == 754018)
  assert(search2(parseBusesWithPosition("67,x,7,59,61")) == 779210)
  assert(search2(parseBusesWithPosition("67,7,x,59,61")) == 1261476)
  assert(search2(parseBusesWithPosition("1789,37,47,1889")) == 1202161486)

  assert(search3(busesWithPosition) == 1068781)
  assert(search3(parseBusesWithPosition("17,x,13,19")) == 3417)
  assert(search3(parseBusesWithPosition("67,7,59,61")) == 754018)
  assert(search3(parseBusesWithPosition("67,x,7,59,61")) == 779210)
  assert(search3(parseBusesWithPosition("67,7,x,59,61")) == 1261476)
  assert(search3(parseBusesWithPosition("1789,37,47,1889")) == 1202161486)

  println("OK")
}