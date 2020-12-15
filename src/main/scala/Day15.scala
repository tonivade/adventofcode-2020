package adventofcode

object Day15 {

  def spoke(initial: Int*): Stream[(Map[Int, Int], Int, Int)] = {
    val ilist = List(initial:_*)
    val imap = List(initial:_*).zipWithIndex.map { case (v, i) => (v, i + 1) }.toMap

    val state = (imap, ilist.last, ilist.size)

    Stream.iterate(state) { 
      case(map, last, current) => {
        if (current % 10000 == 0) println(current)
        if (map.contains(last)) {
          val spoken = current - map(last)
          (map + (last -> current), spoken, current + 1)
        } else
          (map + (last -> current), 0, current + 1)
      }
    }
  }

  def game(nth: Int)(initial: Int*): Int =
    spoke(initial:_*).dropWhile { case (_, _, pos) => pos < nth}.take(1).head._2
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
  
//  assert(game(30000000)(0, 3, 6) == 175594)

  println("OK")
}