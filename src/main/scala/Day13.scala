package adventofcode

import scala.io.Source

object Day13 {

  def search(timestamp: Int, buses: Seq[Int]): (Int, Int) = {
    buses.map(b => (b, b - timestamp % b)).minBy(_._2)
  }
}

object Day13Part1 extends App {
  import Day13._

  val input = Source.fromResource("buses.txt").getLines().toVector
  val timestamp = input(0).toInt
  val buses = input(1).split(",").filterNot(_ == "x").map(_.toInt)
  
  val (busId, min) = search(timestamp, buses)

  println(busId * min)
}

object Day13Test extends App {
  import Day13._

  val timestamp = 939
  val buses = "7,13,x,x,59,x,31,19".split(",").filterNot(_ == "x").map(_.toInt)

  val (busId, min) = search(timestamp, buses)

  assert(busId * min == 295)

  println("OK")
}