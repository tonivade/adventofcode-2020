package adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day13 {
  
  val input = Source.fromResource("buses.txt").getLines().toVector

  def parseBuses(line: String): Seq[Int] =
    line.split(",").filterNot(_ == "x").map(_.toInt)

  def parseBusesWithPosition(line: String): Seq[(Int, Int)] =
    line.split(",").zipWithIndex.filterNot(_._1 == "x").map(t => (t._1.toInt, t._2)).toVector

  def search(timestamp: Int, buses: Seq[Int]): (Int, Int) =
    buses.map(b => (b, b - timestamp % b)).minBy(_._2)

  @tailrec
  def search2(buses: Seq[(Int, Int)], m: Long = 1): Long = {
    val x = buses.head._1 * m
    
    val a = buses.tail.map(t => (x + t._2) % t._1).toVector

    if (a.forall(_ == 0)) {
      println(m)
      x
    }
    else
      search2(buses, m + 1)
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

  println(search2(buses))
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

  println("OK")
}