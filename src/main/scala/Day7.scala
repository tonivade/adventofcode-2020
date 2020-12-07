package adventofcode

import scala.io.Source

object Day7 {

  val input = Source.fromFile("src/main/resources/bags.txt").getLines()

}

object Day7Test extends App {

  import Day7._

  input.foreach(println)

}