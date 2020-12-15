package adventofcode

import scala.annotation.tailrec

object Day15 {

  def spoke(limit: Int)(initial: Seq[Int]): (Map[Int, Int], Int, Int) = {

    @tailrec
    def loop(map: Map[Int, Int], last: Int, current: Int): (Map[Int, Int], Int, Int) = {
      
      if (current % 100000 == 0) println(current)

      if (current == limit) 
        (map, last, current)
      else if (map.contains(last)) {
        val spoken = current - map(last)
        loop(map + (last -> current), spoken, current + 1)
      } else
        loop(map + (last -> current), 0, current + 1)
    }
    
    val state = initial.zipWithIndex.map { case (v, i) => (v, i + 1) }.toMap

    loop(state, initial.last, initial.size)
  }

  def game(nth: Int)(initial: Int*): Int = spoke(nth)(initial)._2
}

object Day15Part1 extends App {
  import Day15._

  println(game(2020)(11,0,1,10,5,19))
}

object Day15Part2 extends App {
  import Day15._

  println(game(30000000)(11,0,1,10,5,19))
}

object Day15Test extends App {
  import Day15._

  assert(game(2020)(0, 3, 6) == 436)
  assert(game(2020)(1, 3, 2) == 1)
  assert(game(2020)(2, 1, 3) == 10)
  assert(game(2020)(1, 2, 3) == 27)
  assert(game(2020)(2, 3, 1) == 78)
  assert(game(2020)(3, 2, 1) == 438)
  assert(game(2020)(3, 1, 2) == 1836)
  
  assert(game(30000000)(0, 3, 6) == 175594)

  println("OK")
}