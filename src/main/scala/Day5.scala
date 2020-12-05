package adventofcode

import scala.io.Source

object Day5 {

  def calculateSeat(line: String): (Int, Int) = {
    val (row, col) = line.splitAt(7)

    val (rmin, rmax) = search(0, 127, row, 'F') 
    val (cmin, cmax) = search(0, 7, col, 'L') 

    assert(rmin == rmax)
    assert(cmin == cmax)
    (rmin, cmin)
  }

  def search(start: Int, end: Int, line: String, lo: Char): (Int, Int) = 
    line.foldLeft((start, end))((state, c) => {
      val (min, max) = state
      val half = ((max - min) / 2) + 1
      if (c == lo)
        (min, max - half)
      else
        (min + half, max)
    })

  def seatId(seat: (Int, Int)) = (seat._1 * 8) + seat._2
  
  val seatIds = Source.fromFile("src/main/resources/seats.txt").getLines().map(calculateSeat).map(seatId).toList
}

object Day5Part1 extends App {
  import Day5._

  println("Day5 Part1")

  println(seatIds.max)
}

object Day5Part2 extends App {
  import Day5._

  println("Day5 Part2")

  val missing = (seatIds.min to seatIds.max).filterNot(seatIds.contains)
    .filter(m => seatIds.contains(m + 1) && seatIds.contains(m - 1)).head

  println(missing)
}

object Day5Test extends App {
  import Day5._

  println("Day5 Test")

  val (row, col) = calculateSeat("FBFBBFFRLR")
  val id = seatId(row, col)

  assert(row == 44)
  assert(col == 5)
  assert(id == 357)
}