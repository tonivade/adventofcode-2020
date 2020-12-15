package adventofcode

object Day15 {

  def spoke(initial: Int*): Stream[(Map[Int, Int], Int, Int)] = {
    val ilist = List(initial:_*)
    val imap = List(initial:_*).zipWithIndex.map { case (v, i) => (v, i + 1) }.toMap

    val state = (imap, ilist.last, ilist.size)

    Stream.iterate(state) { 
      case(map, last, current) => {
        if (map.contains(last)) {
          val spoken = current - map(last)
          (map + (last -> current), spoken, current + 1)
        } else
          (map + (last -> current), 0, current + 1)
      }
    }
  }
}

object Day15Part1 extends App {
  import Day15._

  println(spoke(11,0,1,10,5,19).take(2020 - 5).last._2)
}

object Day15Test extends App {
  import Day15._

  assert(spoke(0, 3, 6).take(2018).last._2 == 436)
  assert(spoke(1, 3, 2).take(2018).last._2 == 1)
  assert(spoke(2, 1, 3).take(2018).last._2 == 10)
  assert(spoke(1, 2, 3).take(2018).last._2 == 27)
  assert(spoke(2, 3, 1).take(2018).last._2 == 78)
  assert(spoke(3, 2, 1).take(2018).last._2 == 438)
  assert(spoke(3, 1, 2).take(2018).last._2 == 1836)

  println("OK")
}