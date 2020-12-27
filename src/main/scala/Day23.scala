package adventofcode

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object Day23 {

  def circular[A](a: Seq[A]): Stream[A] = {
    val repeat = a.toStream
    def b: Stream[A] = repeat #::: b
    b
  }

  /*
   * Vector will contain on each position the value of the next element
   * 
   * For example: 3, 8, 9, 1, 2, 5, 4, 6, 7
   *  
   * |0|1|2|3|4|5|6|7|8|9|
   * ---------------------
   * |3|2|5|8|6|4|7|3|9|1|
   * 
   * Next step will be: 2, 8, 9, 1, 5, 4, 6, 7, 3
   *  
   * |0|1|2|3|4|5|6|7|8|9|
   * ---------------------
   * |2|5|8|2|6|4|7|3|9|1|
   * 
   */
  case class CircularList(array: Vector[Int]) {

    def toVector: Vector[Int] = take(0, array.size - 1)

    def move: CircularList = {
      val current = array(0)
      val taken = take(current, 3)
      var target = search(current - 1, taken).orElse(search(array.size - 1, taken)).getOrElse(0)

      assert(target > 0, s"invalid target: $target")

      val next = array
        .updated(0, array(taken.last))        // the first should be the next after the last taken
        .updated(target, taken.head)          // the target should point to the first taken
        .updated(taken.last, array(target))   // the last taken should point the next from target
        .updated(current, array(taken.last))  // and the current should point to the next after the taken to complete the circle

      CircularList(next)
    }

    def take(from: Int, n: Int): Vector[Int] = 
      if (n > 0) {
        val current = array(from)
        current +: take(current, n - 1) 
      }
      else 
        Vector.empty

    def search(target: Int, taken: Vector[Int]): Option[Int] =
      if (target < 1)
        None
      else if (taken.contains(target))
          search(target - 1, taken)
        else
          Some(target)

    def label: String = take(1, array.size - 2).mkString
  }

  object CircularList {

    def apply(items: Int*): CircularList = {
      val (res, last) = items.foldLeft((Array.fill(items.size + 1)(0), 0)) {
        case ((array, previous), current) =>
          array(previous) = current
          (array, current)
      }

      // last should point to first to complete the circle
      res(last) = items.head

      CircularList(res.toVector)
    }

    def apply(limit: Int)(items: Int*): CircularList = {
      val padding = for {
        i <- (items.max + 1) to limit
      } yield (i)
      
      val padded = items ++ padding

      CircularList(padded:_*)
    }
  }

  def play(limit: Int)(input: CircularList): CircularList =
    if (limit > 0)
      play(limit - 1)(input.move)
    else
      input
}

object Day23Part1 extends App {
  import Day23._

  val result = play(100)(CircularList(9, 6, 2, 7, 1, 3, 8, 5, 4))

  println(result.label)
}

object Day23Part2 extends App {
  import Day23._

  val result = play(10000000)(CircularList(1000000)(9, 6, 2, 7, 1, 3, 8, 5, 4))

  println(result.take(1, 2).foldLeft(1L)(_ * _))
}

object Day23Test extends App {
  import Day23._

  val list1 = CircularList(3, 8, 9, 1, 2, 5, 4, 6, 7)

  assert(list1.array == Vector(3, 2, 5, 8, 6, 4, 7, 3, 9, 1))
  assert(list1.toVector == Vector(3, 8, 9, 1, 2, 5, 4, 6, 7))

  assert(list1.move == CircularList(2, 8, 9, 1, 5, 4, 6, 7, 3))
  assert(list1.move.move == CircularList(5, 4, 6, 7, 8, 9, 1, 3, 2))
  assert(list1.move.move.move == CircularList(8, 9, 1, 3, 4, 6, 7, 2, 5))

  val result10 = play(10)(list1)
  assert(result10 == CircularList(8, 3, 7, 4, 1, 9, 2, 6, 5))
  assert(result10.label == "92658374")
  
  val result100 = play(100)(list1)
  assert(result100.label == "67384529")

  val result10M = play(10000000)(CircularList(1000000)(3, 8, 9, 1, 2, 5, 4, 6, 7))
  assert(result10M.take(1, 2) == Vector(934001, 159792))
  assert(result10M.take(1, 2).foldLeft(1L)(_ * _) == 149245887792L)

  println("OK")
}