package adventofcode

import scala.annotation.tailrec

object Day23 {

  def circular[A](a: Seq[A]): Stream[A] = {
    val repeat = a.toStream
    def b: Stream[A] = repeat #::: b
    b
  }

  case class CircularList(list: List[Int]) {

    def move: CircularList = {
      val current = list.head
      val (taken, rest) = take(3)
      val destination = search(current - 1, rest)

      val (a, b) = rest.splitAt(destination)
      
      val res = ((a :+ b.head) ++ taken ++ b.tail) :+ current

      CircularList(res)
    }

    def take(n: Int): (List[Int], List[Int]) = 
      (
        circular(list).drop(1).take(n).toList,
        circular(list).drop(n + 1).take(list.size - n - 1).toList
      )

    def search(target: Int, rest: List[Int]): Int =
      if (target < 1)
        rest.indexOf(rest.max)
      else 
        rest.indexOf(target) match {
          case x if x < 0 => search(target - 1, rest)
          case x => x
        }

    def label: String = {
      val x = list.indexOf(1)

      val y = circular(list).drop(x + 1).take(list.size - 1).toList

      y.mkString
    }
  }

  object CircularList {
    def apply(items: Int*): CircularList = CircularList(List(items:_*))
  }

  def play(limit: Int)(input: CircularList): CircularList = {
    if (limit > 0)
      play(limit - 1)(input.move)
    else
      input
  }
}

object Day23Part1 extends App {
  import Day23._

  val result = play(100)(CircularList(9, 6, 2, 7, 1, 3, 8, 5, 4))

  println(result.label)
}

object Day23Test extends App {
  import Day23._

  val list1 = CircularList(3, 8, 9, 1, 2, 5, 4, 6, 7)

  assert(list1.move == CircularList(2, 8, 9, 1, 5, 4, 6, 7, 3))
  assert(list1.move.move == CircularList(5, 4, 6, 7, 8, 9, 1, 3, 2))
  assert(list1.move.move.move == CircularList(8, 9, 1, 3, 4, 6, 7, 2, 5))

  val result10 = play(10)(list1)
  assert(result10 == CircularList(8, 3, 7, 4, 1, 9, 2, 6, 5))
  assert(result10.label == "92658374")
  
  val result100 = play(100)(list1)
  assert(result100.label == "67384529")

  println("OK")
}